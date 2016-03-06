module Tagger(tagger) where

import Preprocess(preprocess)
import Parser
import Testing
import Viterbi
import Control.Monad(forever)

interactive_tagger :: IO()
interactive_tagger = forever $ do
   putStrLn "Input the sentence you wish to tag:"
   input <- getLine
   let prepPairs = "preprocess.txt"
   preprocess ["WSJ-2-12"] prepPairs -- works only if file doesn't exist
   (bigramProbs, wordTagProbs) <- Parser.parse prepPairs
   let tagged_stn = viterbi (words input) bigramProbs wordTagProbs
   print tagged_stn
   return ()

test_tagger :: IO ()
test_tagger = do
    runTests "WSJ-2-12"
    print "Testing done"

tagger :: IO ()
tagger = forever $ do
    putStrLn "Press x for interactive or t for testing"
    args <- getLine
    if args == "x" then interactive_tagger
                   else test_tagger
    return ()
