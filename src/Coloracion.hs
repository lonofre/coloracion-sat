module Coloracion where

import Color
import Grafica
import SAT.MiniSat
import qualified Data.Map as Map

type Coloracion = [String]

grafica1 :: Grafica
grafica1 = [ ("a", ["b", "c"]), ("b", ["a", "c"]), ("c", ["a", "b"]) ]

grafica2 :: Grafica
grafica2 = [ ("a", ["b"]), ("b", ["a", "c"]), ("c", ["b"]) ]


-- Genera la fórmula que indica que cada vertice
-- tiene al menos un color: (p1 \/ p2 \/ p3) /\ (q1 \/ q2 \/ q3) ...
verticesTienenColores :: Grafica -> Colores -> Formula String
verticesTienenColores grafica colores = error "o:"


-- Genera la fórmula que indica que dos vértices adyacentes no pueden
-- tener el mismo color: (p1 -> ¬q1 /\ ¬r1 /\ ¬s1) ...
adyDiferenteColor :: Grafica -> Colores -> Formula String
adyDiferenteColor grafica colores = error ":o"

-- Genera la formula proposicional a resolver
formulaColoracion :: Int -> Grafica -> Formula String
formulaColoracion k grafica = parte1 :&&: parte2
                        where colores = generaColores k
                              parte1 = verticesTienenColores grafica colores
                              parte2 = adyDiferenteColor grafica colores

-- Devuelve el resultado de MiniSat a una lista de Coloración
modelosAColoracion :: [Map.Map String Bool] -> [Coloracion]
modelosAColoracion r = error "0:"

-- Devuleve todas las k coloraciones que se pueden realizar
-- con la gráfica
kColoracion  :: Int -> Grafica -> [Coloracion]
kColoracion k =  modelosAColoracion . solve_all . formulaColoracion k