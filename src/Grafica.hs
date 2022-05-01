module Grafica(
    Vertice,
    Adyacencias,
    Grafica
) where

type Vertice = String
type Adyacencias = [Vertice]

type Grafica = [(Vertice, Adyacencias)]

--Regresa una lista con los nombres de los vértices de la gráfica
vertices :: Grafica -> [Vertice] 
vertices grafica = [x | (x,_) <- grafica]
