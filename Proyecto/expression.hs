{-# LANGUAGE PolyKinds, RankNTypes, KindSignatures,
    DataKinds, TypeInType, GADTs  #-}

module Expression where 

    import Coalgebra
    import Streams

    data Expr :: * -> * where
        Var :: Stream a -> Expr a
        Repeat :: a -> Expr a
        Plus :: (Num a) => Expr a -> Expr a -> Expr a
        Nat :: Expr Integer

    instance Coalgebra (Expr) where
        head (Var s) = Coalgebra.head s
        head (Repeat a) = a
        head (Plus e1 e2) = Coalgebra.head e1 + Coalgebra.head e2
        head Nat = 0
        tail (Var s) = Var (Coalgebra.tail s)
        tail (Repeat a) = Repeat a
        tail (Plus e1 e2) = Plus (Coalgebra.tail e1) (Coalgebra.tail e2)
        tail Nat = Plus Nat (Repeat 1)

    eval :: Expr a -> Stream a
    eval = unfold

    repeat k = eval (Repeat k)
    plus s1 s2 = eval (Plus (Var s1) (Var s2))
    nat = eval (Nat)