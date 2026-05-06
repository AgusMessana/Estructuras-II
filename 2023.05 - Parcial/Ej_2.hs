module Ej_2 where

-- =============================================================================
-- # Ejercicio 2
-- =============================================================================

type Interval = (Int, Int)

data ITree = E | N ITree Interval ITree

-- ### Inciso a): dado un árbol no vacío devuelva el extremo derecho del intervalo de más a la derecha del árbol.
right :: ITree -> Int
right (N _ (_, b) E) = b
right (N _ _ der) = right der

-- ### Inciso b): dado un valor de tipo ITree chequee que éste sea un árbol de intervalo.
checkIT :: ITree -> Bool
checkIT E = True
checkIT (N E (a, b) E) = a <= b
checkIT (N E (a, b) der) = a <= b && left der >= b + 1 && checkIT der
checkIT (N izq (a, b) E) = a <= b && right izq <= a - 1 && checkIT izq
checkIT (N izq (a, b) der) = a <= b && right izq <= a - 1 && left der >= b + 1 && checkIT izq && checkIT der

left :: ITree -> Int
left (N E (a, _) _) = a
left (N izq _ _) = left izq

-- ### Inciso c): dadu un árbol de intervalo t devuelva el intervalo que está más a la derecha en t y el árbol t sin ese elemento
splitMax :: ITree -> (Interval, ITree)
splitMax (N izq (a, b) E) = ((a, b), izq)
splitMax (N izq (a, b) der) =
  let (maximo, nuevaRamaDer) = splitMax der
   in (maximo, N izq (a, b) nuevaRamaDer)

-- ### Inciso d): dado un árbol de intervalo N l i r, la función devuelve un árbol de intervalo con los elementos de l y r.
merge :: ITree -> ITree -> ITree
merge E arbol = arbol
merge arbol E = arbol
merge izq der = N nuevaRamaIzq nuevaRaiz der
  where
    (nuevaRaiz, nuevaRamaIzq) = splitMax izq

-- ### Inciso e): dado un árbol y un entero elimine a éste del árbol.
delElem :: ITree -> Int -> ITree
delElem E _ = E
delElem (N izq (a, b) der) x
  | x < a = N (delElem izq x) (a, b) der
  | x > b = N izq (a, b) (delElem der x)
  | x == a && x == b = merge izq der
  | x == a = N izq (a + 1, b) der
  | x == b = N izq (a, b - 1) der
  | otherwise = merge (N izq (a, x - 1) E) (N E (x + 1, b) der)