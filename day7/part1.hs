brute :: Integer -> Integer -> Bool -> [Integer] -> Bool 
brute target current _ [] = target == current 
brute target current first (x:xs) = brute target (current + x) False xs 
                                 || brute target (if first then x else current * x) False xs

main = do
    inp <- readFile "input"
    print $ sum $ map head $ filter (\(x:xs) -> brute x 0 True xs) $ map (map read) $ map words $ map (filter (/= ':')) $ lines inp

