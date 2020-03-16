module Foldables where
    import Data.List

    concat :: [[a]] -> [a]
    concat [] = []
    concat xs = foldr (++) [] xs

    minimum :: (Ord a) => [a] -> a
    minimum [] = error "Empty list"
    minimum (x:xs) = foldr (\x y -> if x < y then x else y) x xs

    reverse :: [a] -> [a]
    reverse [] = []
    reverse xs = foldr (\x acum -> acum ++ [x]) [] xs

    filter :: (a -> Bool) -> [a] -> [a]
    filter _ [] = []
    filter f xs = foldr (\x acum -> if f x then x:acum else acum) [] xs

    inits :: [a] -> [[a]]
    inits [] = []
    inits xs = foldr (\x acum -> [[x]] ++ acum) [] xs

    foldi :: (a -> a) -> a -> Int -> a
    foldi f q 0 = q
    foldi  f q i = f (foldi f q (pred i))

    add :: Int -> Int -> Int
    add x y = foldi (\a -> a + 1) x y

    mult :: Int -> Int -> Int
    mult x y = foldr (\b acum -> add acum b) 0 (foldi (\a -> a ++ [x]) [] y)

    expt :: Int -> Int -> Int
    expt x y = foldr (\b acum -> mult acum b) 1 (foldi (\a -> a ++ [x]) [] y)

    sumq :: Int -> Int
    sumq n = foldr (\b acum -> acum + (expt b 2)) 0 [0..(n - 1)]

    remove :: (Eq a) => [a] -> [a] -> [a]
    remove xs ys = foldr (\x l -> if elem x xs then l else [x] ++ l) [] ys

    remdups :: (Eq a) => [a] -> [a]
    remdups xs = foldr go (`seq` []) xs Nothing
        where
            go x r (Just prev)
                | x == prev = r (Just x)
            go x r _ = x : r (Just x)

    rotate :: [a] -> [[a]]
    rotate [] = [[]]
    rotate (x:xs) = foldr (++) [] (map (interleave [] x) (rotate xs))
        where
            interleave :: [a] -> a -> [a] -> [[a]]
            interleave xs x [] = [xs ++ [x]]
            interleave xs x (y:ys) =
                (xs ++ (x:y:ys)):(interleave (xs ++ [y]) x ys)

    unmerge :: (Ord a) => [a] -> [([a], [a])]
    