module Parser(parseLoop,tagTagCounts,wordTagCounts,showWordTags,tcounts,save,showTags) where

import System.IO
import qualified Data.Map as M
import Data.List(foldl')
import Data.List.Split(splitOn)

-- ================================================================================
-- ================================== COUNTS ===============================
-- ================================================================================

mapFold :: Foldable t => t a -> (M.Map k a1 -> a -> M.Map k a1) -> M.Map k a1
mapFold ls f = foldl' f M.empty ls

-- | 'genProb' computes P(w|t) = Count(w|tag) / Count(tag)
--  or P(t-1|t) = Count(t-1,t) / Count(t) depending on the maps given
--  It also does add-1 smoothing by adding one to all counts
genProb :: (String,String) -> M.Map (String,String) Int  -> M.Map String Int -> Float
genProb (w,t) wcm tm = case M.lookup (w,t) wcm of
    Just c  -> (log $ fromIntegral (c+1)) - (log ct)
    Nothing -> (log $ fromIntegral 1) - (log ct)
    where ct = fromIntegral $ case M.lookup t tm of Just t' -> t'

-- ================================== COUNT TAGi|TAGi-1 ===============================
-- TODO: function for P(tn = j | tn-1 = i ) = Count(t = i, t = j) / Count(t = i)
-- This is also known as the tag transition distribution when we multiply over
-- all possible tags.

-- | 'tagTagCounts' builds the map (tn,tn+1) -> int
tagTagCounts :: [(String,String)] -> M.Map (String,String) Int -> M.Map (String,String) Int
tagTagCounts [x] m = m
tagTagCounts (wt:wts) m = tagTagCounts wts (M.insertWith (+) tt 1 m)
                                  where tt = (snd wt, snd $ head wts)

-- ================================== COUNT WORD|TAG ===============================
-- Word emission distribution when we multiply over all possible pairings

wordTagCounts :: [(String,String)] -> M.Map (String, String) Int -> M.Map (String, String) Int
wordTagCounts [] m = m
wordTagCounts (wt:wts) m = wordTagCounts wts (M.insertWith (+) wt 1 m)

showWordTags :: M.Map (String, String) Int -> [String]
showWordTags m = map prettyWTags $ M.toList m
    where
          prettyWTags :: ((String, String),Int) -> String
          prettyWTags ((w,t),v) = w ++ "|" ++ t ++ "--> " ++ (show v)

-- ================================== COUNT TAGS ===============================
-- | 'tcounts' generates a Map Tag -> Count from the list of word/tag pairs
tcounts :: [(String,String)] -> M.Map String Int -> M.Map String Int
tcounts [] m = m
tcounts ((_,key):sts) m = tcounts sts (M.insertWith (+) key 1 m)

-- | 'showTags' pretty prints the tag counts map
showTags :: M.Map String Int -> [String]
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
