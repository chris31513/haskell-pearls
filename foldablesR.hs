module FoldablesR where

    digits :: Int -> [Int]
    digits 0 = []
    digits x = digits (x `div` 10) ++ [x `mod` 10]

    factorial :: Int -> Int
    factorial 0 = 1
    factorial 1 = 1
    factorial n = n * (factorial (n - 1))

    factorion :: Int -> Int
    factorion x = foldr (+) 0 (map (\y -> factorial y) (digits x))

    iflip :: Int -> Int
    iflip x = foldl (\x y -> 10 * x + y) 0 (foldr (\y acum -> acum ++ [y]) [] (digits x))

    toBin :: Int -> [Int]
    toBin 0 = [0]
    toBin n 
        | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1] 
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

    binarios :: [Int] -> [Int]
    binarios xs = map (\y -> foldl (\x y -> 10 * x + y) 0 y) (map (\x -> toBin x) [1..4])