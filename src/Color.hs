module Color(
    Color,
    Colores,
    generaColores
) where

type Color = String

type Colores = [String]

generaColores :: Int -> Colores
generaColores n = [ show x | x <- [1 .. n] ]