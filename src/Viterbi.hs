{-# LANGUAGE BangPatterns #-}

module Viterbi(viterbi) where
import Data.Map(Map)
import qualified Data.Map.Strict as M
import Data.List(foldl',maximumBy)
import Data.Maybe(fromMaybe,mapMaybe)
import Data.Ord(comparing)

import Parser(tag_set)
import Prelude hiding (Word)
import Types
import Debug.Trace

mapFold :: Foldable t => t a -> (M.Map k a1 -> a -> M.Map k a1) -> M.Map k a1 -> M.Map k a1
mapFold ls f m = foldl' f m ls

getProb :: Show k => Ord k => k -> Map k Double -> Double
getProb k m = fromMaybe (error "Bigrams or scores always in") (M.lookup k m)

-- | 'getWordProbs' get all the probabilities associated to a word return 1.0 for all probs if word is unknown otherwise returns the list of probabilities with 0.0 for the unknown cases
getWordProbs :: Word -> WTProbMap -> [Double]
getWordProbs wrd m
    | null probs      = replicate (length tag_set) 1.0 --unknown word so same prob
    | otherwise       = probs'
    where
        probs = mapMaybe (\(w,t) -> (M.lookup (w,t) m)) [(wrd,tag) | tag <- tag_set]
        probs' = map (\(w,t) -> fromMaybe 0.0 (M.lookup (w,t) m)) [(wrd,tag) | tag <- tag_set] --only computed if empty

start :: String
start = "."

-- ============================== ALGORITHM ==============================
-- | 'initScore' initialises the scores map by computing the scores of the first word for all the available tags
--  so this computes the first column of Score
initScore :: Word -> BiProbMap -> WTProbMap -> Scores
initScore first_w bmap wtmap = foldl' (\m (t,score)-> M.insert (t,first_w) score m) M.empty init_scores
    where
          init_scores = zip tag_set $ zipWith (*) start_bigrams wordProbs -- multiply together and set the tag
          start_bigrams = map (`getProb` bmap ) [(t,start)| t <- tag_set] -- get bigrams (t,<start>)
          wordProbs = getWordProbs first_w wtmap  -- get prob of (w,t) for all tags

-- | 'scoresAndBack the main function of the algorithm
viterbi :: Sentence -> BiProbMap -> WTProbMap -> (Scores,BackTrack, TaggedSentence)
viterbi [] _ _        = error "Not enough words to run the algorithm"
viterbi stn bmap wtmap = (s,b,traceBack stn' sb)
    where sb@(s,b)    = scoresAndBack stn initial_score bmap wtmap M.empty
          initial_score = initScore (head stn) bmap wtmap --get the scores for the first word
          stn'  = reverse stn -- to trace back we need to start from the back

-- |'scoresAndBack' this function is the one that attempts to build the scores for every word
scoresAndBack :: Sentence -> Scores -> BiProbMap-> WTProbMap -> BackTrack -> (Scores,BackTrack)
scoresAndBack [] _ _ _ _                              = error "Empty viterbi"
scoresAndBack [_] scores _ _ backp                    = (scores, backp)
scoresAndBack (prev:curr:stn) scores bmap wtmap backp = scoresAndBack (curr:stn) scores' bmap wtmap backp'
    -- for each tag in the tag set get the max score for the current word
    -- i.e. this is one column of the Scores matrix
    where !wordScores                             = map (\tag -> (tag,maxScore prev curr scores bmap wtmap tag)) tag_set
    --  insert the max score into the map for every tag and current word
          !scores'                                = mapFold wordScores (\m (curr_t,(_,score)) -> M.insert (curr_t,curr) score m) scores
    --  store the tag for the maximum score for the given (tag,word)
          !backp'                                 = mapFold wordScores (\m (curr_t,(tag,_)) -> M.insert (curr_t,curr) tag m) backp

getBestScore :: Word -> Scores -> (Tag,Word,Double)
getBestScore last_w scores = maximumBy (comparing (\(_,_,x) -> x)) lastw_scores
    where lastw_scores = map (\(t,w)-> (t,w, fromMaybe (error "should be in") (M.lookup (t,w) scores))) [(t,last_w)| t <- tag_set]

traceBack :: Sentence -> (Scores,BackTrack) -> TaggedSentence
traceBack [] (_,_) = error "Not Enough words"
traceBack (s:stn) (scores, backp) = traceBack' stn (t,w) backp [(w,t)]
     where (t,w,_) = getBestScore s scores

traceBack' :: Sentence -> (Tag,Word) -> BackTrack -> TaggedSentence -> TaggedSentence
traceBack' [] (_,_) _ result = result
traceBack' (s:st) (t,w) backp result = traceBack' st (tag,s) backp ((s,tag) : result)
    where tag = fromMaybe (error "Back not working") (M.lookup (t,w) backp)

-- This is the same as Score(i,j) = max_1^k (Score(k,j-1) * P(ti,tk) * P(wj|ti))
-- | 'maxScore' returns the biggest score for the current word and tag
maxScore :: Word -> Word -> Scores -> BiProbMap -> WTProbMap -> Tag -> (Tag, Double)
maxScore prev_w curr scores bmap wtmap tag = maximumBy (comparing snd) maxScores
        where mult s@(t,_) = (t,curr_score s * bi_prob t)
              curr_score s = getProb s scores         -- get score of (tag,prev_w)
              bi_prob t    = getProb (tag,t) bmap     -- get the (t,t-1) probability
              maxScores    = zipWith (\pwt (t,s) -> (t,pwt*s)) prob_wt tagWords
              prob_wt      = handle_unknown curr wtmap tag -- get the p(w,t) and check if unknown word
              tagWords     = map mult [(ts,prev_w) | ts <- tag_set]

-- | 'handle_unknown' returns a list of p(w,t) is the current (w,t) pair exists
-- otherwise we check to see if the word is unknown. If it is we return a list
-- of all 1.0's otherwise we return a list of the corresponding probs
handle_unknown :: Word -> WTProbMap -> Tag -> [Double]
handle_unknown curr wtmap tag = case M.lookup (curr,tag) wtmap of
     Just x -> replicate (length tag_set) x --word is known so we use same prob
     Nothing -> getWordProbs curr wtmap -- not (word,tag) check if the word is unknown
