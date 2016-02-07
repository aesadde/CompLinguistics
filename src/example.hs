import qualified Data.Map as M


-- foldmap ls = putStrLn  $ foldl add M.empty ls

add :: M.Map Char Int -> Char -> M.Map Char Int
add m key = M.insertWith (+) key 1 m

mapFold al = foldl (\map k -> M.insertWith (+) k 1 map) M.empty al
mapF al = foldl add M.empty al

main = undefined
