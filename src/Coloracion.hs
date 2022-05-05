module Coloracion where

import Color
import Grafica
import SAT.MiniSat
import Data.List
import qualified Data.Map as Map

type Coloracion = [String]

grafica1 :: Grafica
grafica1 = [ ("a", ["b", "c"]), ("b", ["a", "c"]), ("c", ["a", "b"]) ]

grafica2 :: Grafica
grafica2 = [ ("a", ["b"]), ("b", ["a", "c"]), ("c", ["b"]) ]

-- Genera la fórmula que indica que cada vertice
-- tiene al menos un color: (p1 \/ p2 \/ p3) /\ (q1 \/ q2 \/ q3) ...
verticesTienenColores :: Grafica -> Colores -> Formula String
verticesTienenColores grafica colores = Var (unir (obtenerCombinaciones grafica colores))

--Genera una lista que contiene todas las posibles coloraciones para cada vertice
obtenerCombinaciones:: Grafica ->Colores ->[String]
obtenerCombinaciones a b = [x++y++"::||::"|x<-b,y<-(vertices a)]

--Une los elementos de la lista para obtener la formula logica para su operacion
unir:: [String]-> String
unir [] = ""
unir [x] = x
unir (x:xs) = x ++ unir xs

-- Genera una conjunción de todas las fómulas que halla en una lista de formulas
-- Ejemplo, makeConj [p,q,r,s] = p :&&: q :&&: r :&&: s
makeConj :: [Formula String] -> Formula String
makeConj [x]    = x
makeConj (x:xs) = x :&&: (makeConj xs)

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
adyDiferenteColor [(vE,vA)] [c]    = Var (vE ++ c) :->: (mapNC vA c)
adyDiferenteColor [(vE,vA)] (c:cs) = ( Var(vE ++ c) :->: (mapNC vA c) ) :&&: ( adyDiferenteColor [ (vE,vA) ] cs )        
adyDiferenteColor (x:xs) (c:cs)    = ( adyDiferenteColor [x] (c:cs) ) :&&: ( adyDiferenteColor xs (c:cs) )

-- Genera la formula proposicional a resolver
formulaColoracion :: Int -> Grafica -> Formula String
formulaColoracion k grafica = parte1 :&&: parte2
                        where colores = generaColores k
                              parte1 = verticesTienenColores grafica colores
                              parte2 = adyDiferenteColor grafica colores


-- Transforma una lista de la forma [[("v1", True), ("v2", False)]] a [["v1"]],
-- manteniendo las tuplas cuyo valor booleano sea True
filtraYAplana :: [[(String, Bool)]] -> [[String]]
filtraYAplana = map (\listaInterna -> map (\tupla -> fst tupla) 
                $ filter (\tupla -> snd tupla == True) listaInterna )

-- Regresa una lista de representaciones bonitas de coloraciones, dada la lista de modelos
-- y la lista de variables utilizadas
modelosAColoraciones :: [Map.Map String Bool] -> [Coloracion]
modelosAColoraciones = filtraYAplana . map (\x -> Map.assocs x)

-- Devuleve todas las k coloraciones que se pueden realizar con la gráfica
kColoracion :: Int -> Grafica -> [Coloracion]
kColoracion k = modelosAColoraciones . solve_all . formulaColoracion k
