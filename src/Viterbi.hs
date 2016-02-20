module Viterbi where
import Data.Map(Map)
import qualified Data.Map as M
import Data.List(foldl')

type Sentence = [String]
type BiProbMap= Map (String,String) Int
type WTxxiagMap = Map (String,String) Int
type TagsMap = Map String Int
type TaggedSentence = [(String,String)]
type Scores = Map (String,String) Float
type BackTrack = Map (String,String) Float






-- bigram_prob :: TagsMap -> BigramMap -> Map (String,String) Float
-- bigram_prob tmap bmap = mapFold f all_bigrams
--     where f m ((t,t'),c) =

start :: String
start = "."
--
-- Scores is a map (tag,word) -> Float
-- initScore :: String -> Map (String,String) Float
-- initScore w bmap wtmap tagmap = mapFold [(x,y) | y <- [w] ,x <- tag_set] f
--     where f m p@(t,w) = M.insert p (wordTag_prob * tagStart_prob) m
--             where wordTag_prob = genProb (w,t) wtmap tagmap
--                   tagStart_prob = genProb (t,start) bmap tagmap

-- | 'viterbi' a (very naive) implementation of the viterbi Algorithm
-- viterbi :: Sentence -> BigramMap -> WordTagMap -> TagsMap -> TaggedSentence
-- viterbi stn bmap wtmap tmap = viterbiStep (initScore (head stn)) M.empty

viterbiStep :: Sentence -> Scores -> BackTrack -> BigramMap -> WordTagMap -> TagsMap -> TaggedSentence
viterbiStep (st:stn) scores back bmap wtmap tmap =  undefined

-- maxScore :: (String,String) -> Scores -> BigramMap -> WordTagMap -> TagsMap -> Float
-- maxScore (pw,nw) scores bmap wtmap tagmap = maximum [ss * probtt * probWt | ss <- getScores pw]
--     where probtt =
--         getScores pw = foldl' (\ls sc -> case M.lookup )



