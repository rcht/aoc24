-- module RCHT where

import Data.List (sort) 
import Control.Arrow ((>>>))

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)

split :: [(a, a)] -> [[a]]
split l = [a, b] where
    a = map fst l 
    b = map snd l

splitList :: [[a]] -> [[a]]
splitList = map tuplify >>> split

abs'  n | n >= 0 = n
        | otherwise = -n

pairWiseDiff ([], _) = 0
pairWiseDiff (_, []) = 0
pairWiseDiff (a:as, b:bs) = abs' (a - b) + pairWiseDiff (as, bs)  

solve = lines >>> 
        map words >>> 
        map (map read) >>> 
        splitList >>> 
        map sort >>>
        tuplify >>>
        pairWiseDiff >>>
        print

main = getContents >>= solve


