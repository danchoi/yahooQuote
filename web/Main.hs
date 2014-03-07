{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B
import System.Process
import Control.Monad (join)

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
    let timeout :: Int
        timeout = maybe (2000 :: Int) id (join . fmap (fmap fst . B.readInt) $ timeout')
    maybe (writeBS help)
          (\sym -> do 
              resp <- liftIO $ 
                  readProcess "yahooq" [B.unpack sym, "-t", show timeout] []
              writeBS $ B.pack resp) 
          param


