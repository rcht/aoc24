-- module RCHT where

import Data.List (isSuffixOf)
import Data.Char (isDigit, isLower) 

data Token = AlphString String
           | Open
           | Close
           | NumberString String
           | Comma
           | Special
           deriving (Show, Eq)

data Term = Do
          | Dont
          | Mul Integer Integer
          | Junk
          deriving (Show, Eq)


data State = EnabledSum Integer
           | DisabledSum Integer
           deriving (Show, Eq)

-- String to [Token]
parseString :: String -> [Token]
parseString "" = []
parseString (c : cs) | c == '(' = Open : (parseString cs)
                     | c == ')' = Close : (parseString cs)
                     | c == ',' = Comma : (parseString cs)
                     | isDigit c = case parseString cs of (NumberString s): ts -> (NumberString (c: s)) : ts  
                                                          lt -> (NumberString (c:"")) : lt
                     | isLower c || c =='\'' = case parseString cs of (AlphString s): ts -> (AlphString (c: s)) : ts  
                                                                      lt -> (AlphString (c:"")) : lt
                     | otherwise = Special : (parseString cs)


lastN n = reverse . (take n) . reverse 

allPrefixes :: [a] -> [[a]]
allPrefixes xs = [take n xs | n <- [0..length xs]]

windows n = (map (lastN n)) . allPrefixes


-- [Token] to [Term]
tokenHasSuffix :: String -> Token -> Bool
tokenHasSuffix suf (AlphString s) = suf `isSuffixOf` s 
tokenHasSuffix _ _ = False

tokenIsNumeric :: Token -> Bool
tokenIsNumeric (NumberString _) = True
tokenIsNumeric _ = False


tokensAreDont :: [Token] -> Bool
tokensAreDont xs | length xs < 3 = False 
                 | otherwise = all id $ zipWith ($) [(tokenHasSuffix "don't"), (== Open), (==Close)] $ lastN 3  xs

tokensAreDo :: [Token] -> Bool
tokensAreDo xs | length xs < 3 = False 
               | otherwise = all id $ zipWith ($) [(tokenHasSuffix "do"), (== Open), (==Close)] $ lastN 3  xs

tokensAreMult :: [Token] -> Bool
tokensAreMult xs | length xs < 6 = False 
                 | otherwise = all id $ zipWith ($) [(tokenHasSuffix "mul"), (== Open), tokenIsNumeric, (== Comma), tokenIsNumeric, (==Close)] $ lastN 6 xs

tokenToInt :: Token -> Integer
tokenToInt (NumberString s) = read' s

read' :: String -> Integer
read' = read

tokensToTerm :: [Token] -> Term
tokensToTerm tokens | tokensAreDont tokens = Dont
                    | tokensAreDo tokens = Do
                    | tokensAreMult tokens = Mul a b
                    | otherwise = Junk
                    where
                            a = tokenToInt ((lastN 6 tokens) !! 2)   
                            b = tokenToInt ((lastN 6 tokens) !! 4)   


-- [Term] to State
applyTerm :: State -> Term -> State 
applyTerm (EnabledSum sm) (Mul a b) = EnabledSum $ sm + a * b  
applyTerm (DisabledSum sm) (Mul a b) = DisabledSum sm
applyTerm (EnabledSum sm) Dont = DisabledSum sm
applyTerm (DisabledSum sm) Do = EnabledSum sm
applyTerm st _ = st

evaluateTermsAsState :: [Term] -> State
evaluateTermsAsState terms = foldl applyTerm (EnabledSum 0) terms  

unstate :: State -> Integer
unstate (EnabledSum n) = n 
unstate (DisabledSum n) = n

evaluateTerms :: [Term] -> Integer
evaluateTerms = unstate . evaluateTermsAsState 

solve = print . evaluateTerms . (map tokensToTerm) . (windows 6) . parseString

main = do
    s <- getContents
    -- print $ parseString s
    -- print $ map tokensToTerm $ windows 6 $ parseString s
    solve s
