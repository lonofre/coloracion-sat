module Grafica(
    Vertice,
    Adyacencias,
    Grafica
) where

type Vertice = String
type Adyacencias = [Vertice]

type Grafica = [(Vertice, Adyacencias)]