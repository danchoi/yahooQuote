module Main
where
import Control.Exception 
import Text.CSV (parseCSV)
import Data.List (intercalate)
import Data.Aeson hiding (json, json')
import Network.HTTP.Conduit
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import Options.Applicative
import Control.Monad (when)
import System.Exit (exitSuccess)

data Options = Options { 
                  symbol :: String
                , timeoutMilliSec :: Maybe Int
                , csvOnly  :: Bool 
                } deriving (Show)
  
optionsP :: Parser Options 
optionsP = Options 
            <$> argument str (metavar "SYMBOL" <> help "Ticker symbol") 
            <*> (optional $ option ( 
                long "timeout" <> short 't' <> metavar "MSEC" <> help "Timeout in milliseconds"
              ))
            <*> switch ( long "csv-only" <> help "Dump CSV only" )

main :: IO ()
main = do 
    options <- execParser opts 
    json' <- yahooQuote options 
    B.putStrLn json'
  where opts = info (helper <*> optionsP)
          ( fullDesc 
            <> progDesc "Show JSON info for stock ticker from Yahoo"
            <> header "yahooq - Yahoo financial info"
          )

yahooQuote :: Options -> IO B.ByteString
yahooQuote options = do
    when (csvOnly options) $ do
        csv <- fetch (symbol options) (timeoutMilliSec options)
        putStrLn csv
        exitSuccess
    res <- (fmap (addSym (symbol options) . csv2map.string2csv) $ fetch (symbol options) (timeoutMilliSec options) )
             `catch` (\e -> return $ errorMsg $ show (e :: SomeException))
    return . encode $ res
  where errorMsg e = addSym (symbol options) $ Map.fromList [("Error", e)]
        -- retain old value of Symbol if present, otherwise insert it
        addSym sym map = Map.insertWith (\new old -> old) "Symbol" sym map

{-

  Contemplated errors are

    {"Error":"No matching symbol"}

    In case of timeout or no network:
    {"Error":"FailedConnectionException \"download.finance.yahoo.com\" 80"}
    {"Error":"ResponseTimeout"}
    {"Error": "StatusCodeException (Status {statusCode = 502, statusMessage = \"Server Hangup\"})

    NOTE: All error maps have additional field of "Symbol" 

-}

csv2map :: Either String [[String]] -> Map.Map String String
csv2map (Right (xs:_)) = Map.fromList $ zip (map fst codes') xs
csv2map (Left err) = Map.fromList [("Error", err)]
csv2map _ = error "Empty csv"

string2csv :: String -> Either String [[String]]
string2csv s = case parseCSV "" s of
                 Left _  -> Left $ "No matching symbol" 
                 Right csv -> Right csv

fetch :: String -> Maybe Int -> IO String
fetch sym t = do
  request <- parseUrl $ url sym 
  rsp <- withManager $ httpLbs $ request { responseTimeout = ((* 1000) <$> t) }
  return . B.unpack . responseBody $ rsp

url :: String -> String
url sym = "http://download.finance.yahoo.com/d/quotes.csv?s=" ++ sym ++ "&f=" ++ 
          (intercalate "" $ map snd codes')

-- A restricted set of codes; using the full list leads to CSV parsing anomalies

codes' :: [(String, String)]
codes' = [(k, v) | (k, v) <- codes, k `elem` usedFields]
  where usedFields = ["Symbol", "Name", "Last Trade (Price Only)", "Market Capitalization", "52-week Range", "Volume", "P/E Ratio", "EPS Estimate Current Year", "EPS Estimate Next Year", "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", "Previous Close", "Open", "Bid", "Error Indication (returned for symbol changed / invalid)", "Dividend Yield", "EPS Estiamte Next Year", "EPS Estimate Current Year", "Revenue"]
      
-- Yahoo stock ticker field codes

codes :: [(String, String)]
codes = [  
        ("Name", "n"),
        ("Symbol", "s"),
        ("Market Cap (Real-time)", "j3"),
        ("Market Capitalization", "j1"),
        ("1 yr Target Price", "t8"),
        ("200-day Moving Average", "m4"),
        ("50-day Moving Average", "m3"),
        ("52-week High", "k"),
        ("52-week Low", "j"),
        ("52-week Range", "w"),
        ("After Hours Change (Real-time)", "c8"),
        ("Annualized Gain", "g3"),
        ("Ask (Real-time)", "b2"),
        ("Volume", "v"),
        ("Ask Size", "a5"),
        ("Ask", "a"),
        ("Average Daily Volume", "a2"),
        ("Bid (Real-time)", "b3"),
        ("Bid Size", "b6"),
        ("Bid", "b"),
        ("Book Value", "b4"),
        ("Change & Percent Change", "c"),
        ("Change (Real-time)", "c6"),
        ("Change From 200-day Moving Average", "m5"),
        ("Change From 50-day Moving Average", "m7"),
        ("Change From 52-week High", "k4"),
        ("Change From 52-week Low", "j5"),
        ("Change Percent (Real-time)", "k2"),
        ("Change in Percent", "p2"),
        ("Change", "c1"),
        ("Commission", "c3"),
        ("Day's High", "h"),
        ("Day's Low", "g"),
        ("Day's Range (Real-time)", "m2"),
        ("Day's Range", "m"),
        ("Day's Value Change (Real-time)", "w4"),
        ("Day's Value Change", "w1"),
        ("Dividend Pay Date", "r1"),
        ("Dividend Yield", "y"),
        ("Dividend/Share", "d"),
        ("EBITDA", "j4"),
        ("Revenue", "s6"),
        ("EPS Estimate Current Year", "e7"),
        ("EPS Estimate Next Quarter", "e9"),
        ("EPS Estimate Next Year", "e8"),
        ("Earnings/Share", "e"),
        ("Error Indication (returned for symbol changed / invalid)", "e1"),
        ("Ex-Dividend Date", "q"),
        ("Float Shares", "f6"),
        ("High Limit", "l2"),
        ("Holdings Gain (Real-time)", "g6"),
        ("Holdings Gain Percent (Real-time)", "g5"),
        ("Holdings Gain Percent", "g1"),
        ("Holdings Gain", "g4"),
        ("Holdings Value (Real-time)", "v7"),
        ("Holdings Value", "v1"),
        ("Last Trade (Price Only)", "l1"),
        ("Last Trade (Real-time) With Time", "k1"),
        ("Last Trade (With Time)", "l"),
        ("Last Trade Date", "d1"),
        ("Last Trade Size", "k3"),
        ("Last Trade Time", "t1"),
        ("Low Limit", "l3"),
        ("Notes", "n4"),
        ("Open", "o"),
        ("Order Book (Real-time)", "i5"),
        ("P/E Ratio (Real-time)", "r2"),
        ("P/E Ratio", "r"),
        ("PEG Ratio", "r5"),
        ("Percent Change From 52-week High", "k5"),
        ("Percent Change From 200-day Moving Average", "m6"),
        ("Percent Change From 50-day Moving Average", "m8"),
        ("Percent Change From 52-week Low", "j6"),
        ("Previous Close", "p"),
        ("Price Paid", "p1"),
        ("Price/Book", "p6"),
        ("Price/EPS Estimate Current Year", "r6"),
        ("Price/EPS Estimate Next Year", "r7"),
        ("Price/Sales", "p5"),
        ("Shares Owned", "s1"),
        ("Short Ratio", "s7"),
        ("Trade Date", "d2")
       ]

testcsv :: String
testcsv = "41.16,35.0994,37.9459,41.72,21.87,\"21.87 - 41.72\",\"N/A - N/A\",\"-\",39.70,600,39.70,17247400,39.63,300,39.63,12.587,\"+1.38 - +3.61%\",\"+1.38\",+4.5306,+1.6841,-2.09,+17.76,\"N/A - +3.61%\",\"+3.61%\",+1.38,-,39.79,38.68,\"N/A - N/A\",\"38.68 - 39.79\",\"N/A - N/A\",\"- - +3.61%\",\"N/A\",N/A,0.00,1.206B,1.58,0.37,1.80,1.26,\"N/A\",\"N/A\",   963,215,000,-,N/A,\"N/A - N/A\",\"- - -\",-,N/A,-,39.63,\"N/A - <b>39.63</b>\",\"Mar  4 - <b>39.63</b>\",\"3/4/2014\",535,222,\"4:00pm\",-,N/A,41.166B,\"cn\",\"Yahoo Inc.\",\"-\",38.74,\"N/A\",N/A,30.36,3.34,-5.01%,+12.91%,+4.44%,+81.21%,38.25,-,3.04,24.21,21.25,8.49,-,1.20,\"NasdaqNM\",\"YHOO\",\"&nbsp;===+=+&nbsp;\",-"



 
