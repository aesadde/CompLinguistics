module Tagger where

import Preprocess(preprocess)
import System.IO
import Parser
import System.Environment
import System.Exit
import System.Directory(doesDirectoryExist)
import qualified Data.Map as M
import Viterbi

main :: IO()
main = do
   args <- getArgs >>= parseArgs
   putStrLn "===== Preprocessing Files ====="
   let prepPairs = "preprocess.txt"
   preprocess args prepPairs -- works only if file doesn't exist
   (bigramProbs, wordTagProbs) <- Parser.parse prepPairs
   putStrLn "==== Viterbi Init ===="
   -- let input = words  "I am on a stairway to heaven or a highway to hell ."
   let input = words  "My name is John ."
   let (scores,back,tagged_stn) = viterbi input bigramProbs wordTagProbs
   -- let (_,_,t_stn) = viterbi input2 bigramProbs wordTagProbs
   save  "scores1.txt" $ showbig scores
   save  "back1.txt" $ showWordTags back
   print tagged_stn
   -- print t_stn
   return ()

parseArgs :: [String] -> IO String
parseArgs ["-h"] = usage   >> exit
parseArgs ["-v"] = version >> exit
parseArgs [x]    = doesDirectoryExist x >>= \t -> if t then return x else parseArgs []
parseArgs []     = parseArgs["-h"]

usage   = putStrLn "Usage: tac [-vh] [folder..]"
version = putStrLn "Compling Tagger 0.0.0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
