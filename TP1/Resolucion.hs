module Resolucion where

import Data.List (sortBy)

{-
Integrantes del grupo:
\* Becerra, Nicolás
\* Deppen, Nahuel
\* Messana Gullielmi, Agustín
-}

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

  -- A la distancia la hicimos sin la raíz porque el enunciado dice que sólo había que elevarlo al cuadrado
  dist p1 p2 = sum [(coord i p1 - coord i p2) ^ 2 | i <- [0 .. dimension p1 - 1]]

-- ### Inciso a): definir la función que calcula la distancia entre dos puntos.
-- Realizado dentro de la class Punto

-- ### Inciso b): dar las instancias de Punto para Punto2d y Punto3d.
newtype Punto2d = P2d (Double, Double) deriving (Eq, Show)

newtype Punto3d = P3d (Double, Double, Double) deriving (Eq, Show)

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
fromList = auxFromList 0 -- fromList puntos = auxFromList 0 puntos

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
insertar :: (Punto p) => p -> NdTree p -> NdTree p
insertar pNuevo = auxInsertar pNuevo 0
  where
    n = dimension pNuevo

    auxInsertar p ejeEsperado Empty = Node Empty p Empty ejeEsperado
    auxInsertar p _ (Node izq pNodo der ejeActual)
      | coord ejeActual p <= coord ejeActual pNodo =
          Node (auxInsertar p nuevoEje izq) pNodo der ejeActual
      | otherwise =
          Node izq pNodo (auxInsertar p nuevoEje der) ejeActual
      where
        nuevoEje = (ejeActual + 1) `mod` n

-- =============================================================================
-- # Ejercicio 4
-- =============================================================================
buscarMin :: (Punto p) => Int -> NdTree p -> p
buscarMin _ Empty = error "No hay mínimo en un árbol vacío"
buscarMin ejeBuscado (Node izq pNodo der ejeActual)
  | ejeBuscado == ejeActual =
      case izq of
        Empty -> pNodo
        _ -> buscarMin ejeBuscado izq
  | otherwise =
      let menorPunto p1 p2 =
            if coord ejeBuscado p1 < coord ejeBuscado p2
              then p1
              else p2
          candidatoIzq = case izq of
            Empty -> pNodo
            _ -> buscarMin ejeBuscado izq
          candidatoDer = case der of
            Empty -> pNodo
            _ -> buscarMin ejeBuscado der
       in menorPunto pNodo (menorPunto candidatoIzq candidatoDer)

buscarMax :: (Punto p) => Int -> NdTree p -> p
buscarMax _ Empty = error "No hay máximo en un árbol vacío"
buscarMax ejeBuscado (Node izq pNodo der ejeActual)
  | ejeBuscado == ejeActual =
      case der of
        Empty -> pNodo
        _ -> buscarMax ejeBuscado der
  | otherwise =
      let mayorPunto p1 p2 =
            if coord ejeBuscado p1 > coord ejeBuscado p2
              then p1
              else p2
          candidatoIzq = case izq of
            Empty -> pNodo
            _ -> buscarMax ejeBuscado izq
          candidatoDer = case der of
            Empty -> pNodo
            _ -> buscarMax ejeBuscado der
       in mayorPunto pNodo (mayorPunto candidatoIzq candidatoDer)

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar _ Empty = Empty
eliminar p (Node izq pNodo der ejeActual)
  | p == pNodo =
      case (izq, der) of
        (Empty, Empty) -> Empty
        (_, Empty) ->
          let pReemplazo = buscarMax ejeActual izq
           in Node (eliminar pReemplazo izq) pReemplazo der ejeActual
        _ ->
          let pReemplazo = buscarMin ejeActual der
           in Node izq pReemplazo (eliminar pReemplazo der) ejeActual
  | coord ejeActual p <= coord ejeActual pNodo =
      Node (eliminar p izq) pNodo der ejeActual
  | otherwise =
      Node izq pNodo (eliminar p der) ejeActual

-- =============================================================================
-- # Ejercicio 5
-- =============================================================================
type Rect = (Punto2d, Punto2d)

-- ### Inciso a): crear una función que determina si un punto de dos dimensiones está dentro de un rectángulo.
inRegion :: Punto2d -> Rect -> Bool
inRegion p (pMin, pMax) =
  coord 0 p >= coord 0 pMin
    && coord 0 p <= coord 0 pMax
    && coord 1 p >= coord 1 pMin
    && coord 1 p <= coord 1 pMax

-- ### Inciso b): crear una función que dado un conjunto s de puntos en el plano y un rectángulo, encuentre los puntos de s que están dentro del rectángulo dado
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node izq pNodo der ejeActual) rect@(pMin, pMax) =
  let puntosAca =
        if inRegion pNodo rect
          then [pNodo]
          else []
      puntosIzq =
        if coord ejeActual pMin <= coord ejeActual pNodo
          then ortogonalSearch izq rect
          else []
      puntosDer =
        if coord ejeActual pMax > coord ejeActual pNodo
          then ortogonalSearch der rect
          else []
   in puntosAca ++ puntosIzq ++ puntosDer