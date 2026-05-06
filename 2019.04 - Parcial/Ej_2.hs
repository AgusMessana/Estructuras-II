module Ej_2 where

-- =============================================================================
-- # Ejercicio 2
-- =============================================================================
data Treap p k = E | N (Treap p k) p k (Treap p k)

-- ### Inciso a): devuelve la clave asociada a la raiz del árbol
key :: Treap p k -> k
key (N _ _ k _) = k

-- ### Inciso b): devuelve la prioridad máxima del árbol, suponiendo que es un treap
priority :: Treap p k -> p
priority (N _ p _ _) = p

-- ### Inciso c): determina si un árbol binario de tipo Treap p k es un treap.
isTreap :: (Ord k, Ord p) => Treap p k -> Bool
isTreap E = True
isTreap arbol = auxIsTreap arbol Nothing Nothing

auxIsTreap :: (Ord k, Ord p) => Treap p k -> Maybe k -> Maybe k -> Bool
auxIsTreap E _ _ = True
auxIsTreap (N izq p k der) piso techo =
  checkPiso k piso
    && checkTecho k techo
    && checkHeap p izq der
    && auxIsTreap izq piso (Just k)
    && auxIsTreap der (Just k) techo

checkPiso :: (Ord a) => a -> Maybe a -> Bool
checkPiso _ Nothing = True
checkPiso k (Just p) = k > p

checkTecho :: (Ord a) => a -> Maybe a -> Bool
checkTecho _ Nothing = True
checkTecho k (Just t) = k <= t

checkHeap :: (Ord p) => p -> Treap p k -> Treap p k -> Bool
checkHeap _ E E = True
checkHeap x (N _ p _ _) E = x >= p
checkHeap x E (N _ p _ _) = x >= p
checkHeap x (N _ p1 _ _) (N _ p2 _ _) = x >= p1 && x >= p2

-- ### Inciso d): inserta un elemento a un treap
insert :: (Ord k, Ord p) => k -> p -> Treap p k -> Treap p k
insert k p E = N E p k E
insert k p (N izq p' k' der)
  | k <= k' =
      if priority nuevoIzq > p'
        then rotateR arbolArmadoIzq
        else arbolArmadoIzq
  | otherwise =
      if priority nuevoDer > p'
        then rotateL arbolArmadoDer
        else arbolArmadoDer
  where
    nuevoIzq = insert k p izq
    arbolArmadoIzq = N nuevoIzq p' k' der
    nuevoDer = insert k p der
    arbolArmadoDer = N izq p' k' nuevoDer

rotateL :: Treap p k -> Treap p k
rotateL (N l' p' k' (N l p k r)) = N (N l' p' k' l) p k r

rotateR :: Treap p k -> Treap p k
rotateR (N (N l p k r) p' k' r') = N l p k (N r p' k' r')

-- ### Inciso e): dada una clave x y un treap t divide a t en dos treaps más pequeños, uno que contiene los elementos con clave menor que x y otro con los elementos con clave mayor o igual a x
split :: (Ord k, Ord p, Num p) => k -> Treap p k -> (Treap p k, Treap p k)
split _ E = (E, E)
split x arbol@(N izq p k der) = (nuevoIzq, nuevoDer)
  where
    nuevoArbol = insert x (p + 1) arbol
    N nuevoIzq _ _ nuevoDer = nuevoArbol