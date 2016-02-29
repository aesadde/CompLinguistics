module Parser(parse,showWordTags,tcounts,save,showTags,all_bigrams,build_probs,tag_set) where

import System.IO
import Data.Map(Map)
import qualified Data.Map as M
import Data.List.Split(splitOn)
import Data.Maybe(fromMaybe)

tag_set :: [String]
tag_set = ["#" , "$" , "''" , "(" , ")" , "," , "." , ":" , "CC" , "CD" , "DT" , "EX" , "FW" , "IN" , "JJ" , "JJR" , "JJS" , "LS" , "MD" , "NN" , "NNP" , "NNPS" , "NNS" , "PDT" , "POS" , "PRP" , "PRP$" , "RB" , "RBR" , "RBS" , "RP" , "SYM" , "TO" , "UH" , "VB" , "VBD" , "VBG" , "VBN" , "VBP" , "VBZ" , "WDT" , "WP" , "WP$", "WRB" , "``"]

all_bigrams :: Map (String,String) Int
all_bigrams = M.fromList [((x,y),z)| x <- tag_set, y <-tag_set, z <- [1]]

build_probs :: Map (String,String) Int -> Map String Int -> Map (String,String) Float
build_probs bmap tmap = case M.traverseWithKey f bmap of
    Just m -> m
    Nothing -> error "Cannot build bigrams map"
    where f wt _ = Just $ genProb wt bmap tmap

-- ================================================================================
-- ================================== COUNTS ===============================
-- ================================================================================

-- | 'genProb' computes P(w|t) = Count(w|tag) / Count(tag)
--  or P(t-1|t) = Count(t-1,t) / Count(t) depending on the maps given
genProb :: (String, String) -> Map (String,String) Int -> Map String Int -> Float
genProb (w1,w2) m1 m2 = case M.lookup (w2,w1) m1 of
    Just c -> fromIntegral c / log ct
    Nothing -> case M.lookup (w1,w2) m1 of
        Just c -> fromIntegral c / ct
        Nothing -> 0.0 -- wort|tag not available
    where ct = fromIntegral $ fromMaybe 1 (M.lookup w2 m2)

-- ================================== COUNT TAGi|TAGi-1 ===============================
-- This is also known as the tag transition distribution when we multiply over
-- all possible tags.

-- | 'tagTagCounts' builds the map (tn,tn+1) -> int
tagTagCounts :: [(String,String)] -> Map (String,String) Int -> Map (String,String) Int
tagTagCounts []  _ = error "Not enough tags to count"
tagTagCounts [_] m = m
tagTagCounts (wt:wts) m = tagTagCounts wts (M.insertWith (+) tt 1 m)
                                  where tt = (snd wt, snd $ head wts)

-- ================================== COUNT WORD|TAG ===============================
-- Word emission distribution when we multiply over all possible pairings

wordTagCounts :: [(String,String)] -> Map (String, String) Int
wordTagCounts = foldl (\ m wt -> M.insertWith (+) wt 1 m) M.empty

showWordTags :: Show a => Map (String, String) a -> [String]
showWordTags m = map prettyWTags $ M.toList m
    where
          prettyWTags :: Show a => ((String, String),a) -> String
          prettyWTags ((w,t),v) = w ++ "|" ++ t ++ "--> " ++ show v

-- ================================== COUNT TAGS ===============================
-- | 'tcounts' generates a Map Tag -> Count from the list of word/tag pairs
tcounts :: [(String,String)] -> Map String Int -> Map String Int
tcounts [] m = m
tcounts ((_,key):sts) m = tcounts sts (M.insertWith (+) key 1 m)

-- | 'showTags' pretty prints the tag counts map
showTags :: Map String Int -> [String]
showTags m = map prettyTags $ M.toList m
    where
          prettyTags :: (String,Int) -> String
          prettyTags (k,v) = k ++ "--> " ++ show v

-- ================================== PARSING ===============================
-- | 'parseLoop' generates a list of word/tag pairs from a file
parseLoop:: Handle -> [(String, String)] -> IO [(String,String)]
parseLoop inh lst =
    do ineof <- hIsEOF inh
       if ineof
        then return (reverse lst)
        else do inpStr <- hGetLine inh
                let lst' = parsePair inpStr ++ lst
                parseLoop inh lst'

-- | 'parsePair' take a pair from the file and puts it in the correct format (w,tag)
parsePair :: String -> [(String,String)]
parsePair st = if '|' `elem` l
    then let tags = splitOn "|" l in (h,head tags) : [(h,last tags)]
    else [(h,l)]
        where sp = splitOn "<>" st
              h = head sp
              l = last sp

-- | 'save' saves a list of string to a file
save :: FilePath -> [String] -> IO()
save fpath m = do
    outh <- openFile fpath WriteMode
    mapM_ (hPutStrLn outh) m
    hClose outh
-- ================================================================================

parse :: FilePath -> IO (Map (String,String) Float, Map (String,String) Float)
parse fpath = do
   inh <- openFile fpath ReadMode
   pairsList <- parseLoop inh []
   hClose inh
   -- Counts
   let tagCounts = tcounts pairsList M.empty
   let wtCounts = wordTagCounts pairsList
   let ttCounts = tagTagCounts pairsList all_bigrams

   -- Probabilities
   let bigramProbs = build_probs ttCounts tagCounts
   let wordTagProbs = build_probs wtCounts tagCounts
   save  "bigrams.txt" $ showWordTags bigramProbs
   save  "wtProbs.txt" $ showWordTags  wordTagProbs
   return (bigramProbs,wordTagProbs)
