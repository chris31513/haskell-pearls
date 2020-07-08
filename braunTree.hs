module BraunTree (highExt, lowExt, BTree(BVoid)) where

    import BArray

    data BTree a = BVoid | BNode a (BTree a) (BTree a)

    instance (Show a) => (Show (BTree a)) where

        show (BVoid) = "Void"
        show (BNode a (treeL) (treeR)) = "Node " ++ show(a) ++ " (" ++ show(treeL) ++ ")" ++ " " ++ "(" ++ show(treeR) ++ ")"

    isBal :: BTree a -> Bool
    isBal BVoid = True
    isBal (BNode x (treeL) (treeR)) = ((countSubtrees treeL) == (countSubtrees treeR) + 1) && (isBal treeL) && (isBal treeR)

    countSubtrees :: BTree a -> Int
    countSubtrees BVoid = 0
    countSubtrees (BNode a (BVoid) (BVoid)) = 0
    countSubtrees (BNode a (BNode b (treeL1) (treeR1)) (BVoid)) = 1 + (countSubtrees treeL1) + (countSubtrees treeR1)
    countSubtrees (BNode a (BVoid) (BNode b (treeL2) (treeR2))) = 1 + (countSubtrees treeL2) + (countSubtrees treeR2)
    countSubtrees (BNode a (BNode b (treeL1) (treeR1)) (BNode c (treeL2) (treeR2))) = 2 + (countSubtrees treeL1) + (countSubtrees treeR1) + (countSubtrees treeL2) + (countSubtrees treeR2)

    getRoot :: BTree a -> a
    getRoot (BNode a _ _) = a

    copyInTree :: a -> Int -> BTree a -> BTree a
    copyInTree a 0 tree = tree
    copyInTree a 1 tree = (highExt) a tree
    copyInTree a n tree = copyInTree a (n - 1) (highExt a tree)

    fromListAux :: [a] -> Int -> BTree a -> BTree a
    fromListAux [] _ tree = tree
    fromListAux (x:xs) _ (BVoid) = fromListAux xs 0 (BNode x (BVoid) (BVoid))
    fromListAux (x:xs) 0 (BNode a (treeL) (treeR)) = fromListAux xs 1 (BNode a (highExt x treeL) (treeR))
    fromListAux (x:xs) 1 (BNode a (treeL) (treeR)) = fromListAux xs 0 (BNode a (treeL) (highExt x treeR))

    instance BArray (BTree) where

        emptyArray = BVoid

        isEmpty BVoid = True
        isEmpty _ = False

        size BVoid = 0
        size (BNode a (treeL) (treeR)) = if isBal tree then (1 + (sizeL + (sizeL - 1))) else (1 + sizeL + (size treeR)) where 
            tree = (BNode a (treeL) (treeR))
            sizeL = (size treeL)

        BVoid ! _ = error "Index out of bounds"
        (BNode a (treeL) (treeR)) ! 0 = a
        (BNode a (treeL) (treeR)) ! n = if odd n then treeL ! (mod n (ceiling ((fromIntegral n) / 2))) else treeR ! (mod (n - 1) (ceiling ((fromIntegral n) / 2)))

        update BVoid _ _ = error "Index out of bounds"
        update (BNode a (treeL) (treeR)) 0 b = (BNode b (treeL) (treeR))
        update (BNode a (treeL) (treeR)) n b = if odd n then (BNode a (update treeL (n - 1) b) (treeR)) else (BNode a (treeL) (update treeR (n - 1) b))

        lowExt a BVoid = (BNode a (BVoid) (BVoid))
        lowExt a (BNode b (BVoid) (BVoid)) = (BNode a (lowExt b (BVoid)) (BVoid))
        lowExt a (BNode b (treeL) (treeR)) = (BNode a (lowExt b (treeR)) (treeL))

        lowRem BVoid = BVoid
        lowRem (BNode a (BVoid) (BVoid)) = BVoid
        lowRem (BNode a (treeL) (treeR)) = (BNode root (treeR) (lowRem treeL)) where root = getRoot treeL

        highExt a BVoid = (BNode a (BVoid) (BVoid))
        highExt a (BNode b (BVoid) (BVoid)) = (BNode b (BNode a (BVoid) (BVoid)) (BVoid))
        highExt a (BNode b (treeL) (treeR)) = if odd (size tree) then (BNode b (highExt a treeL) (treeR)) else (BNode b (treeL) (highExt a treeR)) where
            tree = (BNode b (treeL) (treeR))

        highRem BVoid = BVoid
        highRem (BNode a (BVoid) (BVoid)) = BVoid
        highRem (BNode a (treeL) (treeR)) = if odd (size tree) then (BNode a (treeL) (highRem treeR)) else (BNode a (highRem treeL) (treeR)) where
            tree = (BNode a (treeL) (treeR))

        copy a n = copyInTree a n (BVoid)

        mapArr _ BVoid = BVoid
        mapArr f (BNode a (BVoid) (BVoid)) = (BNode (f a) (BVoid) (BVoid))
        mapArr f (BNode a (treeL) (treeR)) = (BNode (f a) (mapArr f treeL) (mapArr f treeR))

        fromList list = fromListAux list 0 (BVoid)

        toList BVoid = []
        toList tree = [(getRoot tree)] ++ (toList (lowRem tree))