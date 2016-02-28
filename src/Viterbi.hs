module Viterbi(viterbi,maxScore,getProb,initScore) where
import Data.Map(Map)
import qualified Data.Map as M
import Data.List(foldl')
import Data.Maybe(fromMaybe)

import Parser(tag_set)
import Types

mapFold :: Foldable t => t a -> (M.Map k a1 -> a -> M.Map k a1) -> M.Map k a1 -> M.Map k a1
mapFold ls f m = foldl' f m ls

getProb :: Show k => Ord k => k -> Map k Float -> Float
getProb k m = case M.lookup k m of
    Just x -> x
    Nothing -> 0.0

start :: String
start = "."
-- ============================== ALGORITHM ==============================
-- | 'initScore' initialises the scores map by computing the scores of the first
-- word for all the available tags
initScore :: String -> BiProbMap -> WTProbMap-> Map (String,String) Float
initScore w bmap wtmap = mapFold [(x,w) | x <- tag_set] f M.empty
    where f m p        = M.insert p (prob p) m
          prob (t,w1) = case M.lookup (w1,t) wtmap of
                Just cw -> case M.lookup (t,start) bmap of
                    Just ts -> cw * ts
                    Nothing -> error $ "Cannot find prob for bigram" ++ show t ++ "|" ++ show start
                Nothing -> 0.0 --if there's now word/tag pair then zero

-- | 'viterbi' the main function of the algorithm
viterbi :: Sentence -> BiProbMap -> WTProbMap -> (Scores,BackTrack,[(String,String)])
viterbi [_] _ _        = error "Not enough words to run the algorithm"
viterbi stn bmap wtmap = (scores, backp, traceBack stn' sb)
    where sb@(scores,backp) = viterbi' stn initS bmap wtmap M.empty
          initS        = initScore (head stn) bmap wtmap
          stn'         = reverse stn

-- |'viterbi'' this function is the one that attempts to build the scores for every word
viterbi' :: Sentence -> Scores -> BiProbMap-> WTProbMap -> BackTrack -> (Scores,BackTrack)
viterbi' [] _ _ _ _                              = error "Empty viterbi"
viterbi'[_] scores _ _ backp                     = (scores, backp)
viterbi' (prev:curr:stn) scores bmap wtmap backp = viterbi' (curr:stn) scores' bmap wtmap backp'
    -- for each tag in the tag set get the max score for the current word
    -- i.e. this is one column of the Scores matrix
    where wordScores                             = map (\tag -> (tag,maxScore prev curr scores bmap wtmap tag)) tag_set
    --  insert the max score into the map for every tag and current word
          scores'                                = mapFold wordScores (\m (curr_t,(_,score)) -> M.insert (curr_t,curr) score m) scores
    --  store the tag for the maximum score for the given (tag,word)
          backp'                                 = mapFold wordScores (\m (curr_t,(tag,_)) -> M.insert (curr_t,curr) tag m) backp

getLastScore :: String -> Scores -> (String,String)
getLastScore last_w scores = dropL $ max'' ("","",0.0) $ map look [(t,last_w)| t <- tag_set]
    where look (t,w) = (t,w,fromMaybe 0.0 (M.lookup (t,w) scores))
          max'' s [] = s
          max'' f@(_,_,s) ((t,w,s'):ts) = if s' > s then max'' (t,w,s') ts else max'' f ts
          dropL (t,w,_) = (t,w)

traceBack :: Sentence -> (Scores,BackTrack) -> [(String,String)]
traceBack [] (_,_) = error "Not Enought words"
traceBack (s:stn) (scores, backp) = traceBack' stn bestScore backp [bestScore]
     where bestScore = getLastScore s scores

traceBack' :: Sentence -> (String,String) -> BackTrack -> [(String,String)] -> [(String,String)]
traceBack' [] (_,_) _ result = result
traceBack' (s:st) (t,w) backp result = traceBack' st (tag,s) backp [(s,tag)] ++ result
    where tag = fromMaybe "" (M.lookup (t,w) backp)

-- This is the same as Score(i,j) = max_1^k (Score(k,j-1) * P(ti,tk) * P(wj|ti))
-- | 'maxScore' returns the biggest score for the current word and tag
maxScore :: String -> String -> Scores -> BiProbMap -> WTProbMap -> String -> (String, Float)
maxScore prev_w curr scores bmap wtmap tag = max' (map mult [(ts,prev_w) | ts <- tag_set]) (".",0.0) --map over all tags
        where mult s@(t,_) = (t,curr_score s * bi_prob t * wt_prob)
              curr_score s = getProb s scores         -- get score of (tag,prev_w)
              bi_prob t    = getProb (tag,t) bmap     -- get the (t,t-1) probability
              wt_prob      = getProb (curr,tag) wtmap -- get the (w,t) probability

-- | 'max'' gets the max of a list of pairs comparing only on the snd member
max' :: Ord a => [(String,a)] ->  (String,a) -> (String,a)
max' [] ma              = ma
max' ((t,m):xs) (t',m') = if m > m' then max' xs (t,m) else max' xs (t',m')
