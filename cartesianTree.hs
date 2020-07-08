module CartesianTree where

    data CartT a = Void | Node (CartT a) a (CartT a)

    instance (Show a) => (Show (CartT a)) where

        show Void = "Void"
        show (Node (treeL) a (treeR)) = "(" ++ show(treeR) ++ ") " ++ show(a) ++ " (" ++ show(treeR) ++ ")"

    insert :: (Ord a) => a -> CartT a -> CartT a
    insert x xs = case xs of
        Void -> Node Void x Void
        Node treeL a treeR -> case a `compare` x of
            LT -> Node (insert a treeL) x (treeR)
            EQ -> Node treeL x treeR
            GT -> Node treeL x (insert a treeR)

    cart :: (Ord a) => [a] -> CartT a
    cart = foldr insert Void

    inorder :: CartT a -> [a]
    inorder Void = []
    inorder (Node (Void) a (Void)) = [a]
    inorder (Node (treeL) a (treeR)) = (inorder treeL) ++ [a] ++ (inorder treeR)