module Coloracion where 
    import Data.List
    import System.Random
    import System.IO.Unsafe

    data Color = Rojo | Amarillo | Verde | Azul deriving (Eq, Show)

    data Balcanes = Albania 
                | Bulgaria
                | BosniayHerzegovina
                | Kosovo
                | Macedonia
                | Montenegro
                deriving(Eq, Show)

    type Ady = [(Balcanes, Balcanes)]

    adyacencias :: Ady
    adyacencias = [(Albania, Montenegro), (Albania, Kosovo), (Albania, Macedonia) 
                , (Bulgaria, Macedonia), (BosniayHerzegovina, Montenegro)
                , (Kosovo, Macedonia), (Kosovo, Montenegro)]

    type Coloracion = [(Color, Balcanes)]

    esBuena :: Ady -> Coloracion -> Bool
    esBuena _ [] = True
    esBuena [] _ = True
    esBuena (x:xs) ys = if c1 /= Nothing && c2 /= Nothing then (quitaMaybe c1) /= (quitaMaybe c2) && (esBuena xs ys) else (esBuena xs ys) where c1 = (getColor (fst x) ys)
                                                                                                                                                c2 = (getColor (snd x) ys)

    getColor :: Balcanes -> Coloracion -> Maybe Color
    getColor _ [] = Nothing
    getColor b (x:xs) = if (snd x) == b then Just (fst x) else getColor b xs

    quitaMaybe :: Maybe Color -> Color
    quitaMaybe (Just c) = c

    coloraciones :: Ady -> [Coloracion]
    coloraciones _ = []

    quitaRepetidos :: Coloracion -> Coloracion
    quitaRepetidos [] = []
    quitaRepetidos (x:xs) = union [x] (quitaRepetidos (xs \\ ([(c, (snd x)) | c <- [Rojo, Amarillo, Verde, Azul]] \\ [x])))

    coloracion :: Ady -> Coloracion
    coloracion [] = []
    coloracion (a:ady) = if esBuena (a:ady) (quitaRepetidos c) then c else (coloracion (a:ady)) where c = union ([((quitaIo (colorRandom [Rojo, Amarillo, Verde, Azul])), (fst a))]) (union ([((quitaIo (colorRandom [Rojo, Amarillo, Verde, Azul])), (snd a))]) (coloracion ady))

    balcanEnColoracion :: Balcanes -> Coloracion -> Bool
    balcanEnColoracion _ [] = False
    balcanEnColoracion b (x:xs) = if ((snd x) == b) then True else balcanEnColoracion b xs

    quitaIo :: IO Color -> Color
    quitaIo c = (unsafePerformIO c)

    colorRandom :: [Color] -> IO Color
    colorRandom l = do
        i <- randomRIO (0, length l - 1)
        return $ l !! i