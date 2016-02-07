module Main where

import Lib
import Parser
import Types

main :: IO ()
main = do
    sentences <- getWords
    putStrLn $ head sentences
