{-# LANGUAGE BangPatterns #-}
module Parser where
import System.Environment
import System.IO
import Control.Monad
import System.Directory
import System.FilePath ((</>))
import qualified Data.Map as M
import Data.List(foldl')
import Text.Regex.Posix
import Data.List.Split(splitOn)

import Types
import Preprocess(preprocess)

-- TODO: Build maps with probabilities
-- ================================================================================
-- ================================== PROBABILITIES ===============================
-- ================================================================================

mapFold :: Foldable t => t a -> (M.Map k a1 -> a -> M.Map k a1) -> M.Map k a1
mapFold ls f = foldl' f M.empty ls

-- ================================== COUNT WORD|TAG ===============================
-- TODO: function for P(w|t) = Count(w|tag) / Count(tag) with smoothing
-- probWordCat :: (String,String) -> Float
-- probWordCat (w,t) m = case M.lookup (w,t) m of

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

main = mainLoop

mainLoop = do
   putStrLn "===== Preprocessing Files ====="
   let prepPairs = "preprocess.txt"
   let dir = "../WSJ-2-12"
   preprocess dir prepPairs -- works only if file doesn't exist
   putStrLn "===== Files preprocessed and saved to 'preprocess.txt' ====="
   inh <- openFile prepPairs ReadMode
   pairsList <- parseLoop inh []
   putStrLn "===== Starting Counts ====="
   let tagCounts = tcounts pairsList M.empty
   save "tagCounts.txt" $ showTags tagCounts
   putStrLn "===== Tag Counts generated and saved to 'tagCounts.txt' ====="
   let wtCounts = wordTagCounts pairsList M.empty
   save  "wordTagCounts.txt" $ showWordTags wtCounts
   putStrLn "===== Word/Tag Counts generated and saved to 'wordTagCounts.txt' ====="
   return ()
