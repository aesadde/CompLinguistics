module Preprocess where

import System.Directory
import System.FilePath ((</>))
import System.IO
import Control.Monad
import Types
import Text.Regex.Posix

-- | `preprocess` dumps all the word/tag pairs to a file if it doesn't exists
-- already otherwise it returns without doing any work
preprocess :: FilePath -> FilePath -> IO()
preprocess dir out =  do
    doesFileExist out >>= (\x -> if not x
    then getRecursiveContents dir >>= (\x -> preprocessLoop (tail x)  out)
    else return ())

-- | `preprocessLoop` takes a list of files and processes them
preprocessLoop :: [FilePath] -> FilePath -> IO()
preprocessLoop [] _ = return ()
preprocessLoop (x:xs) outF = do
    inh <- openFile x ReadMode
    outh <- openFile outF AppendMode
    hPutStrLn outh ("<start>" ++ "<>" ++ "<start>")
    mainloop inh outh
    hClose inh
    hClose outh
    preprocessLoop xs outF

-- | 'mainLoop' Preprocesses a file converting each matched string to a pair
-- that is then dumped into the output file
mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   mapM_ (\x -> hPutStrLn outh (fst x ++ "<>" ++ snd x)) (matchPairs inpStr)
                   mainloop inh outh

-- | 'getRecursiveContents' gets all the files recursively starting from the
-- given directory
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

-- | 'matchPairs' only matches WORD/TAG pairs using a regex
matchPairs :: String -> [(String,String)]
matchPairs stn = map pair (match pat stn :: [[String]])
    where pat = makeRegex "([[:graph:]]+)/([[:graph:]]+)" :: Regex
          pair ls = (head $ tail ls, last ls)
