module YahooCache
where


{-
  This program acts as a cache for yahooq. Put it in front of yahooq in a
  pipeline or after it, or both.

    yahooq [sym] | yahooq-cache 

  will cache the successful output or error returned by yahooq and pass through
  the output (like tee).

    yahooq-cache [sym] --since MIN || yahooq sym

  Will retrive the cached JSON for SYM if it exists and is more recent than MIN and
  otherwise run `yahooq sym`



-}

import Database.HDBC
import Database.HDBC.Sqlite3


    res' <- if (sqliteCache options) 
            then do
              dbh <- connect "tickers.db"
              case (Map.lookup "Error" res) of
                  Just "No matching symbol" -> disconnect dbh >> return res
                  Just err -> do 
                      cachedData <- cachedJson dbh (symbol options) 
                      logError dbh (symbol options) err 
                      let res'' = Map.union res cachedData
                      disconnect dbh
                      return res''
                  Nothing -> cacheResult dbh (symbol options) (encode res) >> disconnect dbh >> return res
            else return res

cacheResult :: IConnection c => c -> String -> B.ByteString -> IO ()
cacheResult dbh sym json' = do
    run dbh
        "INSERT OR REPLACE INTO tickers (ticker, jsonData) VALUES (?, ?)"
        [toSql sym, toSql json']

    commit dbh
    return ()
 
logError :: IConnection c => c -> String -> String -> IO ()
logError dbh sym err = do
    run dbh
        "INSERT INTO errors (ticker, error) VALUES (?, ?)"
        [toSql sym, toSql err]
    commit dbh
    return ()

cachedJson :: IConnection c => c -> String -> IO (Map.Map String String)
cachedJson dbh sym = do
    r <- quickQuery' dbh "select jsonData, lastUpdate from tickers where ticker = ? and \
                \ jsonData is not null" [toSql sym]
    case r of 
      [[json, timestamp]] -> do
          let xs = ((decode $ fromSql json) :: Maybe (Map.Map String String))
          case xs of 
              Just xs' -> return $ Map.insert "CACHED" (fromSql timestamp :: String) xs'
              Nothing -> return $ Map.fromList [("CACHE ERROR", "No cached data")]
      _ -> return $ Map.fromList [("CACHE ERROR", "No cached data")]


-- Database

connect :: FilePath -> IO Connection
connect fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh




prepDB :: IConnection conn => conn -> IO ()
prepDB dbh = do
    tables <- getTables dbh
    when (not ("tickers" `elem` tables)) $ do
        hPutStrLn stderr "Creating tickers database"
        run dbh
            "CREATE table tickers ( \
            \ ticker TEXT NOT NULL UNIQUE, \
            \ jsonData TEXT, \
            \ lastUpdate TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP \
            \)" []
        commit dbh
        return ()
    when (not ("errors" `elem` tables)) $ do
        hPutStrLn stderr "Creating errors database"
        run dbh
            "CREATE table errors ( \
            \ ticker TEXT NOT NULL, \
            \ timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP, \
            \ error  TEXT \
            \)" []
        commit dbh
        return ()
    commit dbh


