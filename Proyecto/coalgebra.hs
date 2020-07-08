module Coalgebra where

    class Coalgebra co where
        head :: co a -> a
        tail :: co a -> co a