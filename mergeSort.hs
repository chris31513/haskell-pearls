module MergeSort where

    import Data.List

    mergeSort :: (Ord a) => [a] -> [a]
    mergeSort [] = []
    mergeSort [x] = [x]
    mergeSort xs = mezcla (mergeSort f) (mergeSort s) where (f, s) = parte xs

    parte :: [a] -> ([a], [a])
    parte xs = (take middle xs, drop middle xs) where middle = div (length xs) 2

    mezcla :: (Ord a) => [a] -> [a] -> [a]
    mezcla [] xs = xs
    mezcla xs [] = xs
    mezcla (x:xs) (y:ys) = if x > y then y:(mezcla (x:xs) ys) else x:(mezcla xs (y:ys))