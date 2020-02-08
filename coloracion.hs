module Coloracion where 

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

    inColoracion :: Balcanes -> Coloracion -> Bool
    inColoracion _ [] = False
    inColoracion b (x:xs) = if (snd x) == b then True else inColoracion b xs

    quitaMaybe :: Maybe Color -> Color
    quitaMaybe (Just c) = c