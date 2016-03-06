module Viterbi(viterbi) where
import Data.Map(Map)
import qualified Data.Map as M
import Data.List(foldl')
import Data.Maybe(fromMaybe)

import Parser(tag_set)
import Types

mapFold :: Foldable t => t a -> (M.Map k a1 -> a -> M.Map k a1) -> M.Map k a1 -> M.Map k a1
mapFold ls f m = foldl' f m ls

getProb :: Show k => Ord k => k -> Map k Float -> Float
getProb k m = fromMaybe 0.0 (M.lookup k m)

start :: String
start = "."
-- ============================== ALGORITHM ==============================
-- | 'initScore' initialises the scores map by computing the scores of the first word for all the available tags
--  so this computes the first column of Score
initScore :: String -> BiProbMap -> WTProbMap-> Map (String,String) Float
initScore w bmap wtmap = mapFold with_wt build_score M.empty
    where build_score m (pwt,p)        = M.insert p (pwt * prob p) m
          prob (t,_) = getProb (t,start) bmap
          with_wt = zip uk [(tag,w) | tag <- tag_set]
          uk = handle_init w wtmap

-- | 'handle_init' a simplified version of handle_unknown for the special case of the initial scores
handle_init :: String -> WTProbMap -> [Float]
handle_init curr wtmap =  if l == 0 then replicate (length tag_set) 1.0 else uk
        where (l,uk) = handle_unknown' curr wtmap tag_set (0,[])

-- | 'viterbi' the main function of the algorithm
viterbi :: Sentence -> BiProbMap -> WTProbMap -> (Scores,BackTrack,[(String,String)])
viterbi [_] _ _        = error "Not enough words to run the algorithm"
viterbi stn bmap wtmap = (s,b,traceBack stn' sb)
    where sb@(s,b)    = viterbi' stn initS bmap wtmap M.empty
          initS = initScore (head stn) bmap wtmap
          stn'  = reverse stn

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

getBestScore :: String -> Scores -> (String,String)
getBestScore last_w scores = dropL $ max'' ("","",0.0) $ map look [(t,last_w)| t <- tag_set]
    where look (t,w) = (t,w,fromMaybe 1.0 (M.lookup (t,w) scores))
          max'' s [] = s
          max'' f@(_,_,s) ((t,w,s'):ts) = if s' > s then max'' (t,w,s') ts else max'' f ts
          dropL (t,w,_) = (t,w)

traceBack :: Sentence -> (Scores,BackTrack) -> [(String,String)]
traceBack [] (_,_) = error "Not Enough words"
traceBack (s:stn) (scores, backp) = traceBack' stn bestScore backp [bestScore]
     where bestScore = getBestScore s scores

traceBack' :: Sentence -> (String,String) -> BackTrack -> [(String,String)] -> [(String,String)]
traceBack' [] (_,_) _ result = result
traceBack' (s:st) (t,w) backp result = traceBack' st (tag,s) backp [(s,tag)] ++ result
    where tag = fromMaybe "" (M.lookup (t,w) backp)

-- This is the same as Score(i,j) = max_1^k (Score(k,j-1) * P(ti,tk) * P(wj|ti))
-- | 'maxScore' returns the biggest score for the current word and tag
maxScore :: String -> String -> Scores -> BiProbMap -> WTProbMap -> String -> (String, Float)
maxScore prev_w curr scores bmap wtmap tag = max' maxScores (head maxScores)
        where mult s@(t,_) = (t,curr_score s * bi_prob t)
              curr_score s = getProb s scores         -- get score of (tag,prev_w)
              bi_prob t    = getProb (tag,t) bmap     -- get the (t,t-1) probability
              maxScores    = zipWith (\sc (t,s) -> (t,sc*s)) (handle_unknown curr wtmap tag) $ map mult [(ts,prev_w) | ts <- tag_set] --map over all tags


-- | 'handle_unknown' returns a list of p(w,t) is the current (w,t) pair exists
-- otherwise we check to see if the word is unknown. If it is we return a list
-- of all 1.0's otherwise we return a list of the corresponding probs
handle_unknown :: String -> WTProbMap -> String -> [Float]
handle_unknown curr wtmap tag = case M.lookup (curr,tag) wtmap of
     Just x -> replicate (length tag_set) x
     Nothing -> if l == 0 then replicate (length tag_set) 1.0 else uk
        where (l,uk) = handle_unknown' curr wtmap tag_set (0,[])

handle_unknown' :: String -> WTProbMap -> [String] -> (Int,[Float]) -> (Int,[Float])
handle_unknown'  _    _     []       res = res
handle_unknown' curr wtmap (t:tagS) (l,res) = case M.lookup (curr,t) wtmap  of
        Just x -> handle_unknown' curr wtmap tagS (l + 1, x : res)
        Nothing -> handle_unknown' curr wtmap tagS (l, 0.0 : res)

-- | 'max'' gets the max of a list of pairs comparing only on the snd member
max' :: Ord a => [(String,a)] ->  (String,a) -> (String,a)
max' [] ma              = ma
max' ((t,m):xs) (t',m') = if m > m' then max' xs (t,m) else max' xs (t',m')
