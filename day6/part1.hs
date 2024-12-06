-- module RCHT where

import Data.Maybe (fromJust)
import Data.Array (bounds, array, (!), elems, assocs)
import Data.Array.MArray
import Data.Array.IO (IOArray)
import Data.List (elemIndex)
import System.IO

data State = Obstacle
           | OutOfGrid
           | Normal

strtoElems :: Int -> String -> [((Int, Int), Char)]
strtoElems a s = zip [(a, x) | x <- [0..]] s

strListToElems :: [String] -> [[((Int, Int), Char)]]
strListToElems ss = [strtoElems a s | (a, s) <- zip [0..] ss]

strListToArray ss = let rows = length ss
                        cols = length (head ss)
                        bounds = ((0,0),(rows-1,cols-1))
                        elements = concat $ strListToElems ss in
                    array bounds elements

tuplePlus (a, b) (c, d) = (a + c, b + d)

directions = [(0,1),(1,0),(0,-1),(-1,0)]

nextDir direction = let k = fromJust $ elemIndex direction directions in directions !! ((k + 1) `mod` (length directions))

simulate grid counts (y,x) (r,c) dr | y>r || y<0 || x>c || x<0 = return OutOfGrid  
                                    | grid ! (y,x) == '#' = return Obstacle 
                                    | otherwise = do
                                        writeArray counts (y,x) 1 
                                        print (y,x)
                                        st <- simulate grid counts (tuplePlus (y,x) dr) (r, c) dr
                                        case st of Normal -> return Normal
                                                   Obstacle -> do
                                                        let dr' = nextDir dr in simulate grid counts (tuplePlus (y,x) dr') (r,c) dr'  
                                                        return Normal
                                                   OutOfGrid -> return Normal


main = do
    hehe <- readFile "input"
    let grid = strListToArray $ lines hehe
    let (r, c) = snd $ bounds grid
    counts <- newArray ((0,0),(r,c)) 0 :: IO (IOArray (Int, Int) Int)
    let spos = head [ind | (ind, e) <- assocs grid, e == '^']
    simulate grid counts spos (r,c) (-1,0)
    xd <- getElems counts
    print $ sum xd
