module Parser(parseLoop,tagTagCounts,wordTagCounts,showWordTags,tcounts,save,showTags,all_bigrams,build_probs) where

import System.IO
import Data.Map(Map)
import qualified Data.Map as M
import Data.List(foldl')
import Data.List.Split(splitOn)

mapFold :: Foldable t => t a -> (M.Map k a1 -> a -> M.Map k a1) -> M.Map k a1
mapFold ls f = foldl' f M.empty ls

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
    Just c -> (log $ fromIntegral c) - log ct
    Nothing -> case M.lookup (w1,w2) m1 of
        Just c -> (log $ fromIntegral c) - log ct
    -- Nothing -> error "Couldn't generate probability"
        Nothing -> 0.0
    where ct = fromIntegral $ case M.lookup w2 m2 of Just t' -> t'

-- ================================== COUNT TAGi|TAGi-1 ===============================
-- TODO: function for P(tn = j | tn-1 = i ) = Count(t = i, t = j) / Count(t = i)
-- This is also known as the tag transition distribution when we multiply over
-- all possible tags.

-- | 'tagTagCounts' builds the map (tn,tn+1) -> int
tagTagCounts :: [(String,String)] -> Map (String,String) Int -> Map (String,String) Int
tagTagCounts [x] m = m
tagTagCounts (wt:wts) m = tagTagCounts wts (M.insertWith (+) tt 1 m)
                                  where tt = (snd wt, snd $ head wts)

-- ================================== COUNT WORD|TAG ===============================
-- Word emission distribution when we multiply over all possible pairings

wordTagCounts :: [(String,String)] -> Map (String, String) Int -> Map (String, String) Int
wordTagCounts [] m = m
wordTagCounts (wt:wts) m = wordTagCounts wts (M.insertWith (+) wt 1 m)

showWordTags :: Show a => Map (String, String) a -> [String]
showWordTags m = map prettyWTags $ M.toList m
    where
          prettyWTags :: Show a => ((String, String),a) -> String
          prettyWTags ((w,t),v) = w ++ "|" ++ t ++ "--> " ++ (show v)

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
          prettyTags (k,v) = k ++ "--> " ++ (show v)
-- ================================================================================

-- ================================== PARSING ===============================
-- | 'parseLoop' generates a list of word/tag pairs from a file
parseLoop:: Handle -> [(String, String)] -> IO([(String,String)])
parseLoop inh lst =
    do ineof <- hIsEOF inh
       if ineof
        then return (reverse lst)
        else do inpStr <- hGetLine inh
                let lst' = (parsePair inpStr) ++ lst
                parseLoop inh lst'

-- | 'parsePair' take a pair from the file and puts it in the correct format (w,tag)
parsePair :: String -> [(String,String)]
parsePair st = if elem '|' l
    then let tags = splitOn "|" l in [(h,head tags)] ++ [(h,last tags)]
    else [(h,l)]
        where sp = splitOn "<>" st
              h = head sp
              l = last sp

-- | 'save' saves a list of string to a file
save :: FilePath -> [String] -> IO()
save fpath m = do
    outh <- openFile fpath WriteMode
    mapM (\x -> hPutStrLn outh x) m
    hClose outh
-- ================================================================================
