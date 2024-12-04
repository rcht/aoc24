-- module RCHT where

import Data.Array
import System.IO

strtoElems :: Int -> String -> [((Int, Int), Char)]
strtoElems a s = zip [(a, x) | x <- [0..]] s

strListToElems :: [String] -> [[((Int, Int), Char)]]
strListToElems ss = [strtoElems a s | (a, s) <- zip [0..] ss]

strListToArray ss = let rows = length ss
                        cols = length (head ss)
                        bounds = ((0,0),(rows-1,cols-1))
                        elements = concat $ strListToElems ss in
                    array bounds elements

pattern = "MAS"


mainDiagCnt arr (y, x) | arr ! (y, x) /= pattern !! 1 = 0
                       | arr ! (y - 1, x-1) == pattern !! 0 && arr ! (y + 1, x+1) == pattern !! 2 = 1 
                       | arr ! (y - 1, x-1) == pattern !! 2 && arr ! (y + 1, x+1) == pattern !! 0 = 1 
                       | otherwise = 0

antiDiagCnt arr (y, x) | arr ! (y, x) /= pattern !! 1 = 0
                       | arr ! (y - 1, x+1) == pattern !! 0 && arr ! (y + 1, x-1) == pattern !! 2 = 1 
                       | arr ! (y - 1, x+1) == pattern !! 2 && arr ! (y + 1, x-1) == pattern !! 0 = 1 
                       | otherwise = 0

windowPatternCnt arr (y,x) = mainDiagCnt arr (y,x) * (antiDiagCnt arr (y, x)) 

allWindowSum arr = let (r, c) = snd $ bounds arr in sum [windowPatternCnt arr (y, x) | y <- [1..r-1], x <- [1..c-1]]


main = do 
    handle <- openFile "input" ReadMode
    s <- hGetContents handle
    print $ allWindowSum $ strListToArray $ lines s
