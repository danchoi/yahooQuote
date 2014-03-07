module Main
where
import Options.Applicative
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Aeson
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Exit
import Control.Monad (when)

{-
  This program acts as a cache for yahooq. Put it in front of yahooq in a
  pipeline or after it, or both.

    yahooq [sym] | yahooq-cache 

  will cache the successful output or error returned by yahooq and pass through
  the output (like tee).

    yahooq-cache [sym] --since MIN || yahooq sym

  Will retrive the cached JSON for SYM if it exists and is more recent than MIN and
  otherwise returns exit status 1 and triggers `yahooq sym` 

-}

data Options = Options {
    cacheFetch :: Maybe String  -- ticker symbol; also puts in fetch mode
  , freshness :: Maybe Int      -- fetch if cached data is newer than N minutes
} deriving (Show)

optionsP :: Parser Options
optionsP = Options 
    <$> (optional $ strOption 
      ( long "symbol" <> short 's' 
        <> metavar "SYMBOL" <> help "Fetch mode; provide ticker symbol")
      )
    <*> (optional $ option 
      ( long "freshness" <> short 'f' 
        <> metavar "MIN" <> help "[fetch mode] fetch from cache if under MIN minutes old")
      )

opts = info (helper <*> optionsP)
          ( fullDesc 
            <> progDesc "Caching service helper for yahooq"
            <> header "yahooq-cache"
          )

main :: IO ()
main = do 
    options <- execParser opts 
    dbh <- connect "tickers.db" 
    case options of 
      (Options Nothing _) -> cachingMode dbh 
      (Options (Just sym) freshness') -> fetchMode dbh sym freshness'
    disconnect dbh


-- cachingMode expects JSON on stdin and stores either the error or the Yahoo ticker data
-- in the database

cachingMode :: IConnection c => c -> IO ()
cachingMode dbh = do
    raw <- B.getContents
    B.putStrLn raw -- pass through input to stdout
    let msg = maybe Map.empty id (decode raw :: Maybe (Map.Map String String))
    case Map.lookup "Symbol" msg of
      Nothing -> error "Missing Symbol value in input data" 
      Just sym -> 
        case Map.lookup "Error" msg of
          Just errorMsg -> logError dbh sym errorMsg 
          Nothing -> cacheResult dbh sym raw 


-- fetchMode looks up the symbol in the cache database and returns the JSON if
-- it exists, otherwise exits with exit code 1


fetchMode :: IConnection c => c -> String -> Maybe Int -> IO ()
fetchMode dbh sym freshness' = do
    r <- quickQuery' dbh "select jsonData, timestamp from tickers where ticker = ? and \
                \ jsonData is not null" [toSql sym]
    case r of 
      [[json, timestamp]] -> do
          let xs = ((decode $ fromSql json) :: Maybe (Map.Map String String))
          case xs of 
              Nothing -> exitFailure
              Just xs' -> B.putStrLn $ encode $ Map.insert "CACHED" (fromSql timestamp :: String) xs'
      _ -> exitFailure

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

-- Database creation and connecting

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
            \ timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP \
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

