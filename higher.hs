module Higher where
    import Data.List

    todos :: (a -> Bool) -> [a] -> Bool
    todos p xs = (length xs) == (length (filter p xs))

    alguno :: (a -> Bool) -> [a] -> Bool
    alguno p xs = (length (filter p xs)) >= 1

    toma :: (a -> Bool) -> [a] -> [a]
    toma p xs = filter p xs

    deja :: (a -> Bool) -> [a] -> [a]
    deja p xs = (filter $ not.p) xs

    altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
    altMap f g xs = (rebuildList (map f (pairIndex xs)) (map g (impairIndex xs)))

    pairIndex :: [a] -> [a]
    pairIndex xs = map (xs !! ) [0,2..length xs - 1]

    impairIndex :: [a] -> [a]
    impairIndex xs = map (xs !!) [1,3..length xs - 1]

    rebuildList :: [a] -> [a] -> [a]
    rebuildList xs [] = xs
    rebuildList [] ys = ys
    rebuildList (x:xs) (y:ys) = x : y : (rebuildList xs ys)

    luhn :: [Int] -> Bool
    luhn [] = False
    luhn xs = (mod (foldr (+) 0 (map (\x -> if x > 9 then x - 9 else x) (altMap (*2) (*1) xs))) 10) == 0