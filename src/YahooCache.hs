{-# LANGUAGE OverloadedStrings #-}
module Main
where
import Options.Applicative
import Database.HDBC
import Database.HDBC.MySQL
import Data.Aeson
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import System.Exit
import Control.Monad
import Data.Configurator

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
  , freshness :: Maybe Int      -- minimum cached age seconds
  , dbPath :: String
} deriving (Show)

optionsP :: Parser Options
optionsP = Options 
    <$> (optional $ strOption 
      ( long "symbol" <> short 's' 
        <> metavar "SYMBOL" <> help "Fetch mode; provide ticker symbol")
      )
    <*> (optional $ option 
      ( long "freshness" <> short 'f' 
        <> metavar "SEC" <> help "[fetch mode] minimum cached age in seconds")
      )
    <*> (strOption 
      ( long "dbconf" <> short 'd' <> metavar "PATH" <> help "path to MySQL db configuration. Default is yahooq.cfg" 
        <> value "yahooq.cfg") 
      )

opts = info (helper <*> optionsP)
          ( fullDesc 
            <> progDesc "Caching service helper for yahooq"
            <> header "yahooq-cache"
          )

main :: IO ()
main = do 
    options <- execParser opts 
    config <- load [ Required $ dbPath options ]
    host' <- lookupDefault "localhost" config "mysqlHost" :: IO String
    user' <- lookupDefault "root" config "mysqlUser" :: IO String
    pass' <- lookupDefault "" config "mysqlPassword" :: IO String
    port' <- lookupDefault 3306 config "mysqlPort" :: IO Int
    sock' <- lookupDefault "" config "mysqlUnixSocket" :: IO String
    db' <- lookupDefault "tickers" config "mysqlDatabase" :: IO String

    let conf = defaultMySQLConnectInfo { 
                  mysqlHost = host',
                  mysqlUser = user', 
                  mysqlDatabase = db',
                  mysqlPort = port', 
                  mysqlUnixSocket = sock'
                }
    dbh <- connect conf
    case options of 
      (Options Nothing _ _) -> cachingMode dbh 
      (Options (Just sym) freshness' _) -> do
          -- This will exit 0 if the symbol has a No matching symbol error in the errors table.
          -- THis may be more properly structured as some kind of MonadPlus
          badSymbol dbh sym 
          fetchMode dbh sym freshness'
    disconnect dbh

-- cachingMode expects JSON on stdin and stores either the error or the Yahoo ticker data
-- in the database

cachingMode :: IConnection c => c -> IO ()
cachingMode dbh = do
    raw <- B.getContents
    B.putStrLn raw -- pass through input to stdout
    let msg = maybe Map.empty id (decode raw :: Maybe (Map.Map String String))
    case Map.lookup "Symbol" msg of
      Nothing -> error $ "Missing Symbol value in input data: " ++ B.unpack raw 
      Just sym -> 
        case Map.lookup "Error" msg of
          Just errorMsg -> logError dbh sym errorMsg 
          Nothing -> cacheResult dbh sym raw 


-- badSymbol will output error json if the Symbol has previouly returned a "No
-- matching symbol" error from the live API.

badSymbol :: IConnection c => c -> String -> IO ()
badSymbol dbh sym = do
    r <- quickQuery' dbh 
          "select count(*), max(timestamp) as t from errors where error = 'No matching symbol' and ticker = ?"
          [ toSql sym ]
    case r of
        [[ct, t]] | (fromSql ct :: Int) > 0 -> do
            putStrLn $ "{Error:\"No matching symbol\",\"CACHED\":\"" ++ (fromSql t) ++ "\"}" 
            exitSuccess
        otherwise -> return ()

-- fetchMode looks up the symbol in the cache database and returns the JSON if
-- it exists, otherwise exits with exit code 1

fetchMode :: IConnection c => c -> String -> Maybe Int -> IO ()
fetchMode dbh sym freshness' = do
    -- freshness is in seconds (not minutes so testing is easier)
    let freshnessInSec = maybe (60 * 100) id freshness'
    r <- quickQuery' dbh 
          "select jsonData, timestamp, \
          \ UNIX_TIMESTAMP(CURRENT_TIMESTAMP) - UNIX_TIMESTAMP(timestamp) as diffSeconds \
          \ from tickers where ticker = ? and \
          \ jsonData is not null \
          \ and timestamp > DATE_SUB(CURRENT_TIMESTAMP, INTERVAL ? SECOND)"
          [ toSql sym, toSql freshnessInSec ]
    case r of 
      [[json, timestamp, diffSeconds]] -> do
        let xs = ((decode $ fromSql json) :: Maybe (Map.Map String String))
        case xs of 
            Nothing -> exitFailure
            Just xs' -> B.putStrLn $ encode $ 
              Map.insert "CACHED" (fromSql timestamp :: String) $ 
              Map.insert "CACHED-AGE-SECONDS" (fromSql diffSeconds :: String) $ 
              xs'
      _ -> exitFailure

cacheResult :: IConnection c => c -> String -> B.ByteString -> IO ()
cacheResult dbh sym json' = do
    run dbh
        "REPLACE INTO tickers (ticker, jsonData) VALUES (?, ?)"
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

connect :: MySQLConnectInfo -> IO Connection
connect conf = do
    dbh <- connectMySQL conf 
    return dbh

-- Remember to create table with create.sql
