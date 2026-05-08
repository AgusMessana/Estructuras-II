module Resolucion where

import Data.List (sortBy)

-- Definición del árbol a utilizar
data NdTree p
  = Node
      (NdTree p) -- subárbol izquierdo
      p -- punto
      (NdTree p) -- subárbol derecho
      Int -- eje
  | Empty
  deriving (Eq, Ord, Show)

-- =============================================================================
-- # Ejercicio 1
-- =============================================================================
class Punto p where
  dimension :: p -> Int -- devuelve el número de coordenadas de un punto
  coord :: Int -> p -> Double -- devuelve la coordenada k-ésima de un punto
  dist :: p -> p -> Double -- calcula la distancia entre dos puntos
  dist p1 p2 = sum [(coord i p1 - coord i p2) ^ 2 | i <- [0 .. dimension p1 - 1]]

-- ### Inciso a): definir la función que calcula la distancia entre dos puntos.
-- Realizado dentro de la class Punto

-- ### Inciso b): dar las instancias de Punto para Punto2d y Punto3d.
newtype Punto2d = P2d (Double, Double)

newtype Punto3d = P3d (Double, Double, Double)

instance Punto Punto2d where
  dimension _ = 2
  coord k (P2d (x, y))
    | k == 0 = x
    | k == 1 = y
    | otherwise = error "Índice fuera de rango para punto de dos dimensiones."

instance Punto Punto3d where
  dimension _ = 3
  coord k (P3d (x, y, z))
    | k == 0 = x
    | k == 1 = y
    | k == 2 = z
    | otherwise = error "Índice fuera de rango para punto de tres dimensiones."

-- =============================================================================
-- # Ejercicio 2
-- =============================================================================
fromList :: (Punto p) => [p] -> NdTree p
fromList puntos = auxFromList 0 puntos

auxFromList :: (Punto p) => Int -> [p] -> NdTree p
auxFromList _ [] = Empty
auxFromList nivel puntos = Node arbolIzq mediana arbolDer eje
  where
    n = dimension (head puntos)
    eje = nivel `mod` n

    compararPorEje p1 p2 = compare (coord eje p1) (coord eje p2)
    puntosOrdenados = sortBy compararPorEje puntos

    indiceMitad = length puntosOrdenados `div` 2

    (izq, resto) = splitAt indiceMitad puntosOrdenados

    mediana = head resto
    der = tail resto

    arbolIzq = auxFromList (nivel + 1) izq
    arbolDer = auxFromList (nivel + 1) der

-- =============================================================================
-- # Ejercicio 3
-- =============================================================================
