module Preprocess (preprocess) where

import System.Directory
import System.FilePath ((</>))
import System.IO
import Control.Monad
import Types
import Text.Regex.Posix

preprocess :: FilePath -> FilePath -> IO()
preprocess dir out =  do
    doesFileExist out >>= (\x -> if not x
    then getRecursiveContents dir >>= (\x -> preprocessLoop (tail x)  out)
    else return ())

preprocessLoop :: [FilePath] -> FilePath -> IO()
preprocessLoop [] _ = return ()
preprocessLoop (x:xs) outF = do
    inh <- openFile x ReadMode
    outh <- openFile outF AppendMode
    mainloop inh outh
    hClose inh
    hClose outh
    preprocessLoop xs outF

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   mapM (\x -> hPutStrLn outh x) (matchPairs inpStr)
                   mainloop inh outh

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
--
-- | 'matchPairs' only matches WORD/TAG pairs using a regex
matchPairs :: Sentence -> [String]
matchPairs stn = concat (match pat stn :: [[String]])
    where pat = makeRegex "[a-zA-Z0-9_.,\\/'&`!?:-]+/[-:!?`&'A-Z_.,\\|]+" :: Regex

