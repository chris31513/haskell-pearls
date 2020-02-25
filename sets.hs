module Sets where

  gtrPower2 :: Int -> Int
  gtrPower2 0 = 0
  gtrPower2 n = listLast [y | y <- [0..(n-1)], isPowerOfTwo y]

  listLast :: [a] -> a
  listLast [x] = x
  listLast (_:xs) = listLast xs
  listLast [] = error "Can't do last of an empty list!"

  isPowerOfTwo :: Int -> Bool
  isPowerOfTwo n 
    | 0 <- n    = True
    | 2 <- n    = True
    | n `mod` 2 == 0  = isPowerOfTwo (n `div` 2)
    | otherwise   = False


  inarow :: (Eq a) => [a] -> Int
  inarow [] = 0
  inarow [x] = 1
  inarow (x:xs) = if (x == (head xs)) then 1 + (inarow xs) else (inarow xs)