module Main where

import Lib

main :: IO ()
main = do
    Right db <- readDb

    pdep <- getProjectDependencies
--     print pdep
    print $ getAllIssues db pdep
--     putStrLn $ show db

