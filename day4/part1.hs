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

pattern = "XMAS"

directionSearchIndex :: Int -> (Int, Int) -> (Int, Int) -> Array (Int, Int) Char -> Int 
directionSearchIndex patternIndex (dy, dx) (i1, i2) arr | patternIndex == length pattern = 1 
                                                        | i1 > r || i1 < 0 = 0
                                                        | i2 > c || i2 < 0 = 0
                                                        | arr ! (i1, i2) /= pattern !! patternIndex = 0
                                                        | otherwise = directionSearchIndex (patternIndex + 1) (dy, dx) (i1 + dy, i2 + dx) arr
                                                        where (r, c) = snd $ bounds arr

directionSearch = directionSearchIndex 0

allmatchesArray arr = let (r, c) = snd $ bounds arr in sum [ directionSearch (dy, dx) (i1, i2) arr | dy <- [-1,0,1], dx <- [-1, 0, 1], i1 <- [0..r], i2 <- [0..c] ]

main = do 
    handle <- openFile "input" ReadMode
    s <- hGetContents handle
    print $ allmatchesArray $ strListToArray $ lines s
