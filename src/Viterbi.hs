module Viterbi where
import Data.Map(Map)
import qualified Data.Map as M

tag_set :: [String]
tag_set = ["#" , "$" , "''" , "(" , ")" , "," , "." , ":" , "CC" , "CD" , "DT" , "EX" , "FW" , "IN" , "JJ" , "JJR" , "JJS" , "LS" , "MD" , "NN" , "NNP" , "NNPS" , "NNS" , "PDT" , "POS" , "PRP" , "PRP$" , "RB" , "RBR" , "RBS" , "RP" , "SYM" , "TO" , "UH" , "VB" , "VBD" , "VBG" , "VBN" , "VBP" , "VBZ" , "WDT" , "WP" , "WP$", "WRB" , "``"]

init_bigrams :: Map (String,String) Float
init_bigrams =M.fromList $ zip [(x,y) | x <- tag_set, y <- tag_set] [1,1..]

start :: String
start = "."

-- initScore ::
-- initScore


-- ================================== PROBABILITIES ===============================

genProb :: (String, String) -> Map (String,String) Int -> Map String Int -> Float
genProb (w1,w2) m1 m2 = case M.lookup (w2,w1) m1 of
    Just c -> (log $ fromIntegral c) - log ct
    Nothing -> 0.0
    where ct = fromIntegral $ case M.lookup w2 m2 of Just t' -> t'

bigramProbMap :: Map (String,String) Int -> Map String Int -> Map (String,String) Float
bigramProbMap tags bigrams = undefined

