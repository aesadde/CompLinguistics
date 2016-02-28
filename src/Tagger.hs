module Tagger(tagger) where

import Preprocess(preprocess)
import System.IO
import Parser
import System.Environment
import System.Exit
import System.Directory(doesDirectoryExist)
import qualified Data.Map as M
import Viterbi

tagger :: IO()
tagger = do
   args <- getArgs >>= parse
   putStrLn "===== Preprocessing Files ====="
   let prepPairs = "preprocess.txt"
   preprocess args prepPairs -- works only if file doesn't exist
   putStrLn "===== Files preprocessed and saved to 'preprocess.txt' ====="
   inh <- openFile prepPairs ReadMode
   pairsList <- parseLoop inh []
   putStrLn "===== Starting Counts ====="
   let tagCounts = tcounts pairsList M.empty
   -- save "tagCounts.txt" $ showTags tagCounts
   putStrLn "===== Tag Counts generated and saved to 'tagCounts.txt' ====="
   let wtCounts = wordTagCounts pairsList M.empty
   -- save  "wordTagCounts.txt" $ showWordTags wtCounts
   putStrLn "===== Word/Tag Counts generated and saved to 'wordTagCounts.txt' ====="
   -- Easy way to use add-1 smoothing -> use a table with all possible tag pairs
   -- init to 1
   let ttCounts = tagTagCounts pairsList all_bigrams
   -- save  "tagTagCounts.txt" $ showWordTags ttCounts
   let bigramProbs = build_probs ttCounts tagCounts
   save  "bigrams.txt" $ showWordTags bigramProbs
   let wordTagProbs = build_probs wtCounts tagCounts
   save  "wtProbs.txt" $ showWordTags  wordTagProbs
   putStrLn "==== Viterbi Init ===="
   let input = words $ "My name is John <end>"
   let (scores,back) = viterbi input bigramProbs wordTagProbs
   save  "scores1.txt" $ showWordTags scores
   save  "back1.txt" $ showWordTags back
   return ()

parse :: [String] -> IO (String)
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse [x]    = doesDirectoryExist x >>= \t -> if t then return x else parse []
parse []     = parse["-h"]

usage   = putStrLn "Usage: tac [-vh] [folder..]"
version = putStrLn "Compling Tagger 0.0.0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
