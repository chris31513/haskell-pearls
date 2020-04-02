module Criptoaritmos where
    import Data.List
    import System.Random
    import System.IO.Unsafe
    import Debug.Trace

    criptoaritmos :: String -> [[(Char, Int)]]
    criptoaritmos xs = criptoaritmosAux xs 10 []

    criptoaritmosAux :: String -> Int -> [[(Char, Int)]] -> [[(Char, Int)]]
    criptoaritmosAux s 0 xs = xs
    criptoaritmosAux s n xs = criptoaritmosAux s (n -1) ([(criptoaritmosPrev s)] ++ xs)

    findPlusSymbol :: String -> Int
    findPlusSymbol s = removeMaybe (elemIndex '+' s)

    findEqSymbol :: String -> Int
    findEqSymbol s = removeMaybe (elemIndex '=' s)

    removeMaybe :: (Maybe Int) -> Int
    removeMaybe (Nothing) = error "Not valid statement"
    removeMaybe (Just n) = n

    splitInPlus :: String -> (String, String)
    splitInPlus s = splitAt (findPlusSymbol f) f where f = filter (/= ' ') s

    splitInEq :: String -> (String, String)
    splitInEq s = splitAt (findEqSymbol f) f where f = filter (/= ' ') s

    splitString :: String -> [String]
    splitString s = [(fst l), (fst m) \\ "+", (snd m) \\ "="] where 
        l = (splitInPlus s) 
        m = (splitInEq (snd l))

    quitaIo :: IO Int -> Int
    quitaIo c = (unsafePerformIO c)

    randomInt :: [Int] -> IO Int
    randomInt l = do
        i <- randomRIO (0, length l - 1)
        return $ l !! i

    stringToInt :: String -> [Int]
    stringToInt [x] = [quitaIo (randomInt [0..9])]
    stringToInt (x:xs) = (quitaIo (randomInt [0..9])):(stringToInt xs)

    generateTuples :: String -> [Int] -> Int -> [(Char, Int)]
    generateTuples [] [] _ = []
    generateTuples (x:xs) (y:ys) n = if (n == 0) then if (y == 0) then (x, (quitaIo (randomInt [1..9]))):(generateTuples xs ys (n + 1)) else (x, y):(generateTuples xs ys (n + 1)) else (x, y):(generateTuples xs ys (n + 1))

    charInList :: Char -> [(Char, Int)] -> Bool
    charInList x [] = False
    charInList x (y:ys) = if (x /= (fst y)) then charInList x ys else True

    removeR :: [(Char, Int)] -> [(Char, Int)]
    removeR [] = []
    removeR (x:xs) = if (charInList (fst x) xs) then removeR xs else x:removeR xs

    getAllTuples :: [String] -> [(Char, Int)]
    getAllTuples [] = []
    getAllTuples (x:xs) = g ++ (getAllTuples xs) where
        g = generateTuples x s 0
        s = stringToInt x

    getTupleIndex :: Char -> [(Char, Int)] -> Int
    getTupleIndex x (y:ys) = if (x == (fst y)) then 0 else (getTupleIndex x ys) + 1

    intToString :: Int -> String
    intToString n = case n of
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"

    makeIntPackage :: [(Char, Int)] -> String -> [String]
    makeIntPackage xs [] = []
    makeIntPackage xs (y:ys) = (intToString (snd (xs !! (getTupleIndex y xs)))):(makeIntPackage xs ys)

    makeAllIntPackages :: [(Char, Int)] -> [String] -> [Int]
    makeAllIntPackages xs [] = []
    makeAllIntPackages xs (y:ys) = (read (foldr (++) "" (makeIntPackage xs y)) :: Int):(makeAllIntPackages xs ys)

    isCorrect :: [Int] -> Bool
    isCorrect xs = (((xs !! 0) + (xs !! 1) == (xs !! 2)))

    criptoaritmosPrev :: String -> [(Char, Int)]
    criptoaritmosPrev s = if (isCorrect m) then t else (criptoaritmosPrev s) where 
        m = makeAllIntPackages t x
        t = removeR (getAllTuples x)
        x = splitString s