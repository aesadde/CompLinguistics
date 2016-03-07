module Testing where

import Preprocess(preprocess)
import Parser
import Viterbi
import Types
import Control.Monad(when)
import Data.List(foldl')

test_dirs :: [(String,[String])]
test_dirs = [(tst,filter (/= tst) d) | tst <- dirs, d <- [dirs]]
    where dirs = ["02","03"]
    -- where dirs = ["02","03","04","05","06","07","08","09","10","11","12"]

-- | 'match' gets a tagged set of words and compares them to the given target
match :: [(String,String)] -> [(String,String)] -> (Int,Int) -> (Int,Int)
match []     [] result                        = result
match ((w,t):tagged) ((w',t'):target)  (correct,total)
        | length tagged + 1 /= length target + 1 = error "Sentences not equal"
        | w == w' && t == t' = match tagged target  (correct + 1, total + 1)
        | otherwise          = match tagged target (correct, total +  1)

train :: FilePath -> (String,[String]) -> IO ()
train fpath (test,dirs) = do
     let training = test ++ "_training.txt"
     let testing = test ++ "_test.txt"
     let dirs' = map (\x -> fpath ++ "/" ++ x) dirs
     let test' = fpath ++ "/" ++ test
     preprocess dirs' training -- get all the training data
     preprocess [test'] testing  -- get all the test data
     return()

testSentences :: FilePath -> IO ()
testSentences test = do
     let training = test ++ "_training.txt"
     let testing = test ++ "_test.txt"
     print $ "Testing on " ++ test
     (bigramProbs, wordTagProbs) <- Parser.parse training--get all probs for current training set
     test_set <- Parser.getSentences testing -- get the list of test sentences
     counts <- run_viterbi bigramProbs wordTagProbs test_set []
     let (correct,total) = foldl' (\(c,t) (c',t') -> (c+c',t+t')) (0,0) counts
     let accuracy = 100 * (fromIntegral correct / fromIntegral total)
     print $ "Accuracy for test " ++ test ++ " = " ++ show accuracy ++ "%"
     return()

run_viterbi :: BiProbMap -> WTProbMap -> [(String,[(String,String)])] -> [(Int,Int)]-> IO [(Int,Int)]
run_viterbi _           _            []           counts = return counts
run_viterbi bigramProbs wordTagProbs ((test,validation):test_set) counts = do
     let (_,_,tagged_sentence) = viterbi (words test) bigramProbs wordTagProbs
     let (c,t) = match tagged_sentence validation (0,0)
     -- when (c /= t) $ do
     --    print "Testing sentence: "
     --    print test
     --    print "Tagged as: "
     --    print tagged_sentence
     run_viterbi bigramProbs wordTagProbs test_set ((c,t): counts)

runTests :: FilePath -> IO ()
runTests fpath = do
    mapM_ (train fpath) test_dirs
    mapM_ (\(d,_) -> testSentences d) test_dirs
    return ()
