module GeneralTree where

    import BraunTree

    data Gtree a = Node a [Gtree a]

    showPretty :: [Char] -> [Char]
    showPretty string = if length string > 0 then take ((length string) - 2) string else string

    instance (Show a) => (Show (Gtree a)) where

        show (Node a []) = show(a)
        show (Node a subT) = show(a) ++ " [" ++  showPretty (foldr (\x y -> show(x) ++ ", " ++ y) "" subT) ++ "]"

    size :: Gtree a -> Int
    size (Node a subT) = foldr (\x y -> (size x) + y) 1 subT

    depth :: Gtree a -> Int
    depth (Node a subT) = 1 + maximum (0:(map depth subT))

    tranAux :: Gtree a -> BTree a -> BTree a
    tranAux (Node a subT) tree = foldr (\x y -> (tranAux x y)) (highExt a tree) subT

    tran :: Gtree a -> BTree a
    tran tree = tranAux tree BVoid

    mapg :: (a -> b) -> Gtree a -> Gtree b
    mapg f (Node a subT) = (Node (f a) (foldr (\x y -> [mapg f x] ++ y) [] subT))

    foldg :: (a -> [b] -> b) -> Gtree a -> b
    foldg f (Node a subT) = f a (map (foldg f) subT)

    searching :: (Eq a) => a -> Gtree a -> Bool
    searching e (Node a subT) = (foldr (\x y -> (searching e x) || y) (e == a) subT)