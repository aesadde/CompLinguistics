module Parser where

import System.Environment
import Types
import Control.Monad
import Text.Regex.Posix




removeLines :: Sentences -> Sentences
removeLines = filter (\x -> x /= "" && x /= "======================================")

parseSentences :: FilePath -> IO Sentences
parseSentences fpath = readFile fpath >>= \x -> return (removeLines $ lines x)

-- TODO: GetPairs of word/Tag
matchPairs :: Sentence -> Words
matchPairs stn = concat (match pat stn :: [[String]])
    where pat = makeRegex "[a-zA-Z0-9_.,'&`!?:-]+/[-:!?`&'a-zA-Z0-9_.,]+" :: Regex

getPairs :: Sentences -> Words -> Words
getPairs [] ws = ws
getPairs (st:stns) ws = if (matchPairs st) == [""] then getPairs stns ws else getPairs stns (ws ++ (matchPairs st))





-- TODO: Parse all files

-- TODO: Build maps with probabilieties



main :: IO()
main = do
    args <- getArgs
    --TODO: Need case handling here
    mapM_ putStrLn =<< (parseSentences (head args) >>= (\snts -> return (getPairs snts [])))
