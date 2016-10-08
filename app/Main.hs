module Main where

import Lib

main :: IO ()
main = do
    db <- readDb
    putStrLn $ show db
