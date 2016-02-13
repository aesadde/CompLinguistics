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

mapFold ls f = foldl' f M.empty ls

tcounts :: [(String,String)] -> M.Map String Int -> M.Map String Int
tcounts [] m = m
tcounts ((_,key):sts) m = tcounts sts (M.insertWith (+) key 1 m)

parseLoop:: Handle -> [(String, String)] -> IO([(String,String)])
parseLoop inh lst =
    do ineof <- hIsEOF inh
       if ineof
        then return (reverse lst)
        else do inpStr <- hGetLine inh
                let lst' = (parsePair inpStr) : lst
                parseLoop inh lst'

parsePair :: String -> (String,String)
parsePair st = let sp = splitOn "<>" st in (head sp, last sp)


showTags :: M.Map String Int -> [String]
showTags m = map prettyTags $ M.toList m
    where
          prettyTags :: (String,Int) -> String
          prettyTags (k,v) = k ++ "--> " ++ (show v)

save :: FilePath -> [String] -> IO()
save fpath m = do
    outh <- openFile fpath WriteMode
    mapM (\x -> hPutStrLn outh x) m
    hClose outh

main = do
   putStrLn "===== Preprocessing Files ====="
   let prepPairs = "preprocess.txt"
   let dir = "../WSJ-2-12"
   preprocess dir prepPairs
   inh <- openFile prepPairs ReadMode
   pairsList <- parseLoop inh []
   putStrLn "===== Starting Count building ====="
   let tagCounts = tcounts pairsList M.empty
   save "tagCounts.txt" $ showTags (tcounts pairsList M.empty)
   putStrLn "===== Tag Counts generated and saved to 'tagCounts.txt' ====="
   return ()
