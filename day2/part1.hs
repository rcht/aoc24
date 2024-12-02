-- module RCHT where 

import Control.Arrow ((>>>))

read' :: String -> Int
read' = read

boolToInt b | b == True = 1
            | otherwise = 0

isIncreasingFine [] _ = True
isIncreasingFine (a: as) b | b <= a = False
                           | b >= (a + 4) = False
                           | otherwise = isIncreasingFine as a

isDecreasingFine [] _ = True
isDecreasingFine (a: as) b | a <= b = False
                           | a >= (b + 4) = False
                           | otherwise = isDecreasingFine as a

isFine a = isIncreasingFine a pred || isDecreasingFine a suc where
    pred = 1 + head a 
    suc = (-1) + head a


solve = lines >>>
        map words >>>
        map (map read') >>>
        map isFine >>>
        map boolToInt >>>
        sum >>>
        print

main = getContents >>= solve
