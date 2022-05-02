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


-- Regresa una lista de strings con los nombres de todas las variables proposicionales 
-- utilizadas (cada una formada por la concatenación del string de un vértice 
-- y el string de un color) dada una gráfica y una lista de colores
variableStrings :: Grafica -> Colores -> [String]
variableStrings grafica colores = [x++y | x<-(vertices grafica), y<-colores]

-- Genera la fórmula que indica que cada vertice
-- tiene al menos un color: (p1 \/ p2 \/ p3) /\ (q1 \/ q2 \/ q3) ...
verticesTienenColores :: Grafica -> Colores -> Formula String
verticesTienenColores grafica colores = error "o:"

-- Genera una conjunción de todas las fómulas que halla en una lista de formulas
-- Ejemplo, makeConj [p,q,r,s] = p :&&: q :&&: r :&&: s
makeConj :: [Formula String] -> Formula String
makeConj [x]    = x
makeConj (x:xs) = x:&&:(makeConj xs)

-- Genera la fórmula que indica que todos los vértices que halla en una lista no pueden tener un color en particular
-- Ejemplo, mapNC ["q","r","s"] "0" = Not (Var "q0") :&&: Not (Var "r0") :&&: Not (Var "s0")
mapNC :: [Vertice] -> Color -> Formula String 
mapNC vL c  = makeConj(map (\v -> (Not (Var (v++c))) ) vL) 

-- Genera la fórmula que indica que dos vértices adyacentes no pueden
-- tener el mismo color: (p1 -> ¬q1 /\ ¬r1 /\ ¬s1) ...
-------------------------------------------------------------------------------------------
-- vértice de ejemplo -> x = ("p",["q","r","s"])
-- vE: el elemento del vértice(vertex Element). Ejemplo, el vE de x es "p"
-- vA: las adyascencias del vértice(vertex adjacency). Ejemplo, el vA de x es ["q","r","s"]
adyDiferenteColor :: Grafica -> Colores -> Formula String
adyDiferenteColor [(vE,vA)] [c]    = Var(vE++c):->:(mapNC vA c)
adyDiferenteColor [(vE,vA)] (c:cs) = (Var(vE++c):->:(mapNC vA c)):&&:(adyDiferenteColor [(vE,vA)] cs)        
adyDiferenteColor (x:xs) (c:cs)    = (adyDiferenteColor [x] (c:cs)):&&:(adyDiferenteColor xs (c:cs))

-- Genera la formula proposicional a resolver
formulaColoracion :: Int -> Grafica -> Formula String
formulaColoracion k grafica = parte1 :&&: parte2
                        where colores = generaColores k
                              parte1 = verticesTienenColores grafica colores
                              parte2 = adyDiferenteColor grafica colores

-- Regresa una representación bonita de la coloración correspondiente a un modelo,
-- dado el modelo y la lista de variables utilizadas
modeloAColoracion :: Map String Bool -> [String] -> String
modeloAColoracion m vars = intercalate "," verdades
    where verdades = [x | x<-vars , m!x == True]

-- Regresa una lista de representaciones bonitas de coloraciones, dada la lista de modelos
-- y la lista de variables utilizadas
modelosAColoraciones :: [Map String Bool] -> [String] -> [String]
modelosAColoraciones [] vars = []
modelosAColoraciones (x:xs) vars = (modeloAColoracion x vars) : (modelosAColoraciones xs vars)

-- Devuleve todas las k coloraciones que se pueden realizar con la gráfica
kColoracion :: Int -> Grafica -> [Coloracion]
kColoracion k grafica =  modelosAColoraciones (solve_all formulaColoracion k) (variableStrings grafica (generaColores k))
