{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B
import YahooQuote

main :: IO ()
main = quickHttpServe site


help :: B.ByteString
help = "Use resource pattern: yahooQuote/[symbol]?timeout=[milliseconds]"

site :: Snap ()
site =
    ifTop (writeBS help) <|>
    route [ ("yahooQuote/:sym", yahooHandler) ]


yahooHandler :: Snap ()
yahooHandler = do
    param <- getParam "sym"
    timeout' <- getParam "timeout"
    let timeout = maybe (Just 200) (fmap fst . B.readInt) timeout'
    maybe (writeBS help)
          (\sym -> do 
              resp <- liftIO $ yahooQuote (Options (B.unpack sym) timeout True False) 
              writeLBS resp) 
          param


