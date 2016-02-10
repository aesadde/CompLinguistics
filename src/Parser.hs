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

import Types
import Preprocess(preprocess)

-- TODO: Build maps with probabilities
-- ================================================================================
-- ================================== PROBABILITIES ===============================
-- ================================================================================

mapFold ls f = foldl' f M.empty ls

-- TODO: Rethink how tags get matched

-- | 'tagCounts' matches all tags and builds a map with the counts for the tags
tagCounts :: Sentence -> M.Map String Int
tagCounts stn = mapFold (concat (match pat stn :: [[String]])) add
        where   pat = makeRegex "/+[-:!?`&'A-Z_.,]+" :: Regex
                add :: M.Map String Int -> String -> M.Map String Int
                add m key = M.insertWith (+) key 1 m

tcounts :: [String] -> M.Map String Int -> M.Map String Int
tcounts [] m = m
tcounts (st:sts) m = tcounts sts (M.insertWith (+) key 1 m)
    where key = st =~ "/[-:!?`'A-Z_.,]+" :: String


showTags :: M.Map String Int -> [String]
showTags m = map prettyTags $ M.toList m
    where
          prettyTags :: (String,Int) -> String
          prettyTags (k,v) = k ++ "--> " ++ (show v)

save :: FilePath -> [String] -> IO()
save fpath m = do
    outh <- openFile fpath AppendMode
    mapM (\x -> hPutStrLn outh x) m
    hClose outh

main = do
   let out = "preprocess.txt"
   let dir = "../WSJ-2-12"
   preprocess dir out
   -- inh <- openFile out ReadMode
   -- inpStr <- hGetContents inh
   -- save "tagCounts.txt" $ showTags (tcounts (lines inpStr) M.empty)
   return ()
