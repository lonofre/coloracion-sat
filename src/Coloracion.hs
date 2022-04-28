module Coloracion where

import Color
import Grafica
import SAT.MiniSat

type Coloracion = [(Vertice, Color)]

grafica1 :: Grafica
grafica1 = [ ("a", ["b", "c"]), ("b", ["a", "c"]), ("c", ["a", "b"]) ]

grafica2 :: Grafica
grafica2 = [ ("a", ["b"]), ("b", ["a", "c"]), ("c", ["b"]) ]

-- Devuleve todas las k coloraciones que se pueden realizar
-- con la gráfica
kColoracion  :: Int -> Grafica -> [Coloracion]
kColoracion = error "lol"