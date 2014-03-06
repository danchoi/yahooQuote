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

site :: Snap ()
site =
    ifTop (writeBS "yahooQuote/:sym") <|>
    route [ ("yahooQuote/:sym", yahooHandler) ]


yahooHandler :: Snap ()
yahooHandler = do
    param <- getParam "sym"
    maybe (writeBS "must specify yahooQuote/:sym in URL")
          (\sym -> do 
              resp <- liftIO $ yahooQuote (Options (B.unpack sym) (Just 2000) True) 
              writeLBS resp) 
          param


