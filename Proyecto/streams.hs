module Streams where

    import Coalgebra

    data Stream a = Cons {head::a, tail::Stream a}

    instance (Show a) => (Show (Stream a)) where

        show (Cons a s) = show(a) ++ ", " ++ show(s)

    instance Coalgebra (Stream) where

        head (Cons a s) = a
        tail (Cons a s) = s

    infix 5 ≺
    (≺) :: a -> Stream a -> Stream a
    a ≺ s = Cons a s

    --Uso la ¡ porque no podía usar la γ
    infix 5 ¡
    (¡) :: Stream a -> Stream a -> Stream a
    s ¡ t = Coalgebra.head s ≺ (t ¡ Coalgebra.tail s)

    infix 5 ≺≺
    (≺≺) :: [a] -> Stream a -> Stream a
    [] ≺≺ s = s
    (x:xs) ≺≺ s = x ≺ (xs ≺≺ s)

    (∆) :: (Num a) => Stream a -> Stream a
    (∆) s = Coalgebra.tail s - s

    --Uso # porque no me podía usar Σ
    (#) :: (Num a) => Stream a -> Stream a
    (#) s = t where t = 0 ≺ (t + s)

    infix 7 ×
    (×) :: (Num a) => Stream a -> Stream a -> Stream a
    s × t = ((Coalgebra.head s) * (Coalgebra.head t)) ≺ ((Streams.repeat (Coalgebra.head s)) * ((Coalgebra.tail t) + (Coalgebra.tail s × t)))

    infix 7 ÷
    s ÷ t = s × Streams.recip t

    repeat :: a -> Stream a
    repeat a = s where s = a ≺ s

    map :: (a -> b) -> (Stream a -> Stream b)
    map f s = f (Coalgebra.head s) ≺ Streams.map f (Coalgebra.tail s)

    zip :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
    zip f s t = f (Coalgebra.head s) (Coalgebra.head t) ≺ Streams.zip f (Coalgebra.tail s) (Coalgebra.tail t)

    iterate :: (a -> a) -> (a -> Stream a)
    iterate f a = a ≺ Streams.iterate f (f a)

    const :: (Num a) => a -> Stream a
    const n = n ≺ Streams.repeat 0

    z :: (Num a) => Stream a
    z = 0 ≺ (1 ≺ Streams.repeat 0)

    unfold :: (Coalgebra co) => co a -> Stream a
    unfold s = Coalgebra.head s ≺ unfold (Coalgebra.tail s)

    instance (Num a) => (Num (Stream a)) where

        (+) = Streams.zip (+)
        (-) = Streams.zip (-)
        (*) = Streams.zip (*)
        negate = Streams.map negate
        fromInteger i = Streams.repeat (fromInteger i)
        abs = Streams.map abs
        signum = Streams.map signum


    nat = 0 ≺ (nat + 1)
    fac = 1 ≺ ((nat + 1) * fac)
    fib = 0 ≺ fib'
    fib' = 1 ≺ (fib' + fib)
    bin = 0 ≺ ((2 * bin + 1) ¡ (2 * bin + 2))
    carry = 0 ¡ (carry + 1)
    turn 0 = []
    turn n = turn n ++ [n] ++ turn n
    tree n = n ≺ (turn n ≺≺ tree (n + 1))
    frac = nat ¡ frac
    god = 2 * frac + 1
    jos = (1 ≺ 2 * jos - 1) ¡ (2 * jos + 17)
    ones = 0 ≺ ones + (1 - carry)
    s = (Streams.const (Coalgebra.head s)) + (z × Coalgebra.tail s)
    recip s = t where 
        a = Prelude.recip (Streams.head s)
        t = a ≺ Streams.repeat (-a) * (Streams.tail s × t)