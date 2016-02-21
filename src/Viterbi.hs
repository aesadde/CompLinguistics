module Viterbi(viterbi,maxScore,getProb,initScore) where
import Data.Map(Map)
import qualified Data.Map as M
import Data.List(foldl')
import Parser(tag_set)

import Debug.Trace

type Sentence = [String]
type BiProbMap= Map (String,String) Float
type WTProbMap = Map (String,String) Float
type TagsMap = Map String Int
type TaggedSentence = [(String,String)]
type Scores = Map (String,String) Float
type BackTrack = Map (String,String) String


mapFold :: Foldable t => t a -> (M.Map k a1 -> a -> M.Map k a1) -> M.Map k a1 -> M.Map k a1
mapFold ls f m = foldl' f m ls

getProb :: Show k => Ord k => k -> Map k Float -> Float
getProb k m = case M.lookup k m of
    Just x -> x
    -- Nothing -> error $ "Prob Not found for" ++(show k)
    Nothing -> 0.0

start :: String
start = "."
--
-- Scores is a map (tag,word) -> Float
initScore :: String -> BiProbMap -> WTProbMap-> Map (String,String) Float
initScore w bmap wtmap =  mapFold [(x,w) | x <- tag_set] f M.empty
    where f m p = M.insert p (prob p) m
          prob (t,w)= case M.lookup (w,t) wtmap of
                Just cw -> case M.lookup (t,start) bmap of
                    Just ts -> cw * ts
                    Nothing -> error $ "Cannot find prob for bigram" ++ (show t) ++ "|" ++ (show start)
                Nothing -> 0.0 --if there's now word/tag pair then zero

viterbi :: Sentence -> BiProbMap -> WTProbMap -> (Scores,BackTrack)
viterbi [x] _ _ = error "Not enough words to run the algorithm"
viterbi stn bmap wtmap = algo
    where algo = viterbi' stn initS bmap wtmap M.empty
          initS = initScore (head stn) bmap wtmap

viterbi' :: Sentence -> Scores -> BiProbMap-> WTProbMap -> BackTrack -> (Scores,BackTrack)
viterbi'[x] scores _ _ backp = (scores, backp)
viterbi' (prev:curr:stn) scores bmap wtmap backp = viterbi' (curr:stn) scores' bmap wtmap backp'
    -- for each tag in the tag set get the max score for the current word
    where wordScores = map (\tag -> (tag,maxScore prev curr scores bmap wtmap tag)) tag_set
    --  insert the max score into the map for every tag and current word
          scores'    = mapFold wordScores (\m (curr_t,(tag,score)) -> M.insert (curr_t,curr) score m) scores
          backp'     = mapFold wordScores (\m (curr_t,(tag,_)) -> M.insert (curr_t,curr) tag m) backp


-- | 'maxScore' returns  the biggest score for the current word and tag
maxScore :: String -> String -> Scores -> BiProbMap -> WTProbMap -> String -> (String, Float)
maxScore prev curr scores bmap wtmap tag = max' ((map mult [(ts,prev) | ts <- tag_set])) (".",0.0) --map over all tags
        where mult s@(t,w) = (t,(curr_score s) * (bi_prob t) * wt_prob)
              curr_score s = getProb s scores -- get score for current pair
              bi_prob t    = getProb (t,tag) bmap
              wt_prob      = getProb (curr,tag) wtmap

-- | 'max'' gets the max of a list of pairs comparing only on the snd member
max' :: Ord a => [(String,a)] ->  (String,a) -> (String,a)
max' [] ma = ma
max' ((t,m):xs) (t',m') = if m > m' then max' xs (t,m) else max' xs (t',m')
