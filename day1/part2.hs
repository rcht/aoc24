-- module RCHT where

import Control.Arrow ((>>>))
import Data.Map (fromListWith, toList)

tuplify :: [a] -> (a,a)
tuplify [x,y] = (x,y)

split l = a ++ b where
    a = map (fst >>> (, (1,0))) l 
    b = map (snd >>> (, (0,1))) l

splitList = map tuplify >>> split

abs'  n | n >= 0 = n
        | otherwise = -n

tuplePlus (a, b) (c, d) = (a + c, b + d) 

read' :: String -> Int
read' = read


-- solve :: String -> IO ()
solve = lines >>> 
        map words >>> 
        map (map read') >>> 
        splitList >>> 
        fromListWith tuplePlus >>>
        toList >>>
        map ( \((x, (a,b))) -> x * a * b)>>>
        sum >>>
        print

main = getContents >>= solve



