module Parser(getSentences,parse,showProbs,showPairCounts,tcounts,save,showTags,all_bigrams,tag_set) where

import System.IO
import Data.Map(Map)
import qualified Data.Map.Strict as M
import Data.List.Split(splitOn)
import Data.Maybe(fromMaybe)
import Text.Printf(printf)
import Types
import Prelude hiding (Word)

tag_set :: [Tag]
tag_set = ["#" , "$" , "''" , "(" , ")" , "," , "." , ":" , "CC" , "CD" , "DT" , "EX" , "FW" , "IN" , "JJ" , "JJR" , "JJS" , "LS" , "MD" , "NN" , "NNP" , "NNPS" , "NNS" , "PDT" , "POS" , "PRP" , "PRP$" , "RB" , "RBR" , "RBS" , "RP" , "SYM" , "TO" , "UH" , "VB" , "VBD" , "VBG" , "VBN" , "VBP" , "VBZ" , "WDT" , "WP" , "WP$", "WRB" , "``"]

-- | 'all_bigrams' initialises the list of all t|t pairs to 1  (add-1 smoothing by default)
all_bigrams :: Map (String,String) Int
all_bigrams = M.fromList [((x,y),z)| x <- tag_set, y <-tag_set, z <- [1]]

build_bigram_probs :: Map (String,String) Int -> Map String Int -> Map (String,String) Double
build_bigram_probs bmap tmap = fromMaybe  (error "Cannot build bigrams map") (M.traverseWithKey f bmap)
    where f wt _ = Just $ genBigProb wt bmap tmap

build_wt_probs :: Map (String,String) Int -> Map String Int -> Map (String,String) Double
build_wt_probs bmap tmap = fromMaybe  (error "Cannot build wt map") (M.traverseWithKey f bmap)
    where f wt _ = Just $ genWTProb wt bmap tmap

-- ================================================================================
-- ================================== COUNTS ===============================
-- ================================================================================

-- | 'genProb' computes P(w|t) = Count(w|tag) / Count(tag)
--  P(t-1|t) = Count(t-1,t) / Count(t) depending on the maps given
genBigProb :: (Tag, Tag) -> Map (String,String) Int -> TagsMap -> Double
genBigProb (w1,w2) m1 m2 = case M.lookup (w2,w1) m1 of
    Just c -> fromIntegral c / ct
    Nothing -> error "This should not happen since all bigrams have at least count 1"
    where ct = fromIntegral $ fromMaybe 1 (M.lookup w2 m2)

genWTProb :: (Word, Tag) -> Map (String,String) Int -> TagsMap -> Double
genWTProb (w,t) wtmap tmap = case M.lookup (w,t) wtmap of
    Just c ->  fromIntegral c / ct
    Nothing -> error "W/T"
    where ct = fromIntegral $ fromMaybe 1 (M.lookup t tmap)

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
wordTagCounts :: [(String,String)] -> Map (String, String) Int
wordTagCounts = foldl (\ m wt -> M.insertWith (+) wt 1 m) M.empty

showPairCounts :: Show a => Map (String, String) a -> [String]
showPairCounts m = map prettyWTags $ M.toList m
    where
          prettyWTags :: Show a => ((String, String),a) -> String
          prettyWTags ((w,t),v) = w ++ "|" ++ t ++ " --> " ++ show v

showProbs :: Map (String, String) Double -> [String]
showProbs m = map prettyWTags $ M.toList m
    where
          prettyWTags :: ((String, String),Double) -> String
          prettyWTags ((w,t),v) = w ++ "|" ++ t ++ " --> " ++ printf "%.8f" v

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
--  this handles the cases where a word has more than one tag w/T1|T2
parsePair :: String -> [(Word,Tag)]
parsePair st
    | '|' `elem` l          = let tags = splitOn "|" l in (h,head tags) : [(h,last tags)]
    -- | h == "." && l == "."  = ("<start>","<start>") : [(h,l)]
    | otherwise             = [(h,l)]
        where sp = splitOn "<>" st
              h = head sp
              l = last sp

-- | 'save' saves a list of string to a file
save :: FilePath -> [String] -> IO()
save fpath m = do
    outh <- openFile fpath WriteMode
    mapM_ (hPutStrLn outh) m
    hClose outh
    return ()

getSentences :: FilePath -> IO [(String,TaggedSentence)]
getSentences fpath = do
    inh <- openFile fpath ReadMode
    stns <- getSentences' inh []
    hClose inh
    return stns

getSentences':: Handle -> [(String,TaggedSentence)] -> IO [(String,TaggedSentence)]
getSentences' inh lst =
    do ineof <- hIsEOF inh
       if ineof
        then return lst
        else do
            st <- getSentence inh ("",[])
            getSentences' inh (st : lst)

getSentence :: Handle -> (String,TaggedSentence) -> IO (String,TaggedSentence)
getSentence inh (st,pairs) =
    do ineof <- hIsEOF inh
       if ineof
        then return (st,reverse pairs)
        else do
            inpStr <- hGetLine inh
            let pair  = head $ parsePair inpStr
            let w = fst pair
            let res = (st ++ " " ++ w, pair : pairs)
            if w == "."
            then return (st ++ " " ++ w, reverse (pair: pairs))
            else getSentence inh res

-- ================================================================================

parse :: FilePath -> IO (BiProbMap, WTProbMap)
parse fpath = do
   inh <- openFile fpath ReadMode
   pairsList <- parseLoop inh []
   hClose inh
   -- Counts
   let tagCounts = tcounts pairsList M.empty -- generate the tag counts
   let wtCounts = wordTagCounts pairsList  -- generate the (w,t) counts
   let ttCounts = tagTagCounts pairsList all_bigrams -- generate the (t,t) counts with add-1 smoothing

   -- Probabilities
   let bigramProbs = build_bigram_probs ttCounts tagCounts
   let wordTagProbs = build_wt_probs wtCounts tagCounts
   -- save  "bigrams.txt" $ showProbs bigramProbs
   -- save  "wtProbs.txt" $ showProbs wordTagProbs
   return (bigramProbs,wordTagProbs)
