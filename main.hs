import System.Environment
main :: IO()
main = do
    args <- getArgs
    --TODO: Need case handling here
    file <- readFile $ head args
    mapM_ putStrLn (lines file)
