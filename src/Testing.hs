module Testing where

import Preprocess(preprocess)
import System.IO
import Parser
import System.Environment
import System.Exit
import System.Directory(doesDirectoryExist)
import qualified Data.Map as M
import Viterbi

test_dirs :: [(String,[String])]
test_dirs = [("WSJ-2-12/"++test, map ("WSJ-2-12/"++) $ filter (/= test) d) | test <- dirs, d <- [dirs]]
    where dirs = ["02","03","04","05","06","07","08","09","10","11","12"]

-- | 'match' gets a tagged set of words and compares them to the given target
match :: [(String,String)] -> [(String,String)] -> (Int,Int) -> (Int,Int)
match []     [] result                        = result
match ((w,t):tagged) ((w',t'):target)  (correct,total)
        | w == w' && t == t' = match tagged target  (correct + 1, total + 1)
        | otherwise          = match tagged target (correct, total)

train (test,dirs) = do
     let training = test ++ "_training.txt"
     let testing = test ++ "_test.txt"
     preprocess dirs training -- get all the training data
     preprocess [test] testing  -- get all the test data

     (bigramProbs, wordTagProbs) <- Parser.parse training--get all probs for current training set
     return()
