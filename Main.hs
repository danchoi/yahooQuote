module Main where
import YahooQuote

main = do 
    r <- fetch "YHOO"
    print r
