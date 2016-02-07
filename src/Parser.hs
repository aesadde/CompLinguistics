{-# LANGUAGE BangPatterns #-}
module Parser (getWords) where

import System.Environment
import System.IO
import Types
import Control.Monad
import Text.Regex.Posix
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Map.Strict as M

-- ================================================================================
-- ================================== PARSER -- ===================================
-- ================================================================================
--
-- | 'removeLines' removes specific unwanted lines
-- removeLines :: Sentences -> Sentences
-- removeLines = BC.filter (\x -> x /= '' && x /= "======================================")

-- | 'parseSentences' reads the given filepath and returns the relevant sentences
parseSentences :: FilePath -> IO Sentences
parseSentences fpath = B.readFile fpath >>= \x -> return $ (BC.lines x)

-- | The 'parseFiles' function parses all the files in the WSJ directory
parseFiles :: IO Sentences
parseFiles = (liftM concat) $ mapM parseSentences ["../WSJ-2-12/"++ n ++"/WSJ_"++ n ++ m ++ ".POS"  | n <- nn, m <- mm ]
    where nn = (map (\x -> "0" ++ show x) [2..9])
          mm = (map (\x -> "0" ++ show x) [0..9]) ++ map show [10..99]

-- | 'matchPairs' only matches WORD/TAG pairs using a regex
matchPairs :: Sentence -> Words
matchPairs stn = concat (match pat stn :: [[B.ByteString]])
    where pat = makeRegex "[a-zA-Z0-9_.,'&`!?:-]+/[-:!?`&'a-zA-Z0-9_.,]+" :: Regex

-- | 'getPairs' takes a list of sentences and a list of words and returns all
-- pairs word/tag pairs in the sentences
getPairs :: Sentences -> Words -> Words
getPairs [] ws = ws
getPairs (st:stns) ws = let mt = matchPairs st in
                            if mt == [BC.empty] then getPairs stns ws
                            else getPairs stns (ws ++ mt)

-- | 'getWords' simply parses all the fails and gets all the word/tag pairs
getWords :: IO Sentence
getWords = (parseFiles >>= (\snts -> return (BC.unlines $ getPairs snts [])))


-- TODO: Build maps with probabilities
-- ================================================================================
-- ================================== PROBABILITIES ===============================
-- ================================================================================



mapFold ls f = foldl f M.empty ls

-- | 'tagCounts' matches all tags and builds a map with the counts for the tags
tagCounts :: Sentence -> M.Map B.ByteString Int
tagCounts stn = mapFold (concat (match pat stn :: [[B.ByteString]])) add
        where   pat = makeRegex "/[-:!?`&'a-zA-Z0-9_.,]+" :: Regex
                add :: M.Map B.ByteString Int -> B.ByteString -> M.Map B.ByteString Int
                add m key = M.insertWith (+) key 1 m

-- prettyPrintTagCounts :: M.Map B.ByteString Int -> IO ()
-- prettyPrintTagCounts  m = do
--         putStrLn "========== Tag Counts =========="
--         putStrLn $ head [tag ++ " --> " ++ count | (tag,count) <- (M.fromList m)]



main = do
    sentence <- getWords
    -- BC.putStrLn sentence
    tags <- return $ tagCounts sentence
    putStrLn $ show tags
