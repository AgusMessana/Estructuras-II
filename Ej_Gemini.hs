module Ej_Gemini where

data ZTree a = E | Z a Bool Int Int (ZTree a) (ZTree a) deriving (Show, Eq)

{-
a: valor del nodo
Bool: estado (True = vivo, False = zombie)
Int1: tamaño total del subárbol (vivos + zombies)
Int2: cantidad de hijos zombies
ZTree a 1: hijo izquierdo
ZTree a 2: hijo derecho
-}

-- ### a): devuelve el tamaño total del árbol
size :: ZTree a -> Int
size E = 0
size (Z _ _ ht _ _ _) = ht

-- ### b): devuelve la cantidad de hijos muertos
zombies :: ZTree a -> Int
zombies E = 0
zombies (Z _ _ _ hm _ _) = hm

-- ### c): buscar como un BST normal, pero solo devuelve True si el elemento está en el árbol Y está vivo
member :: (Ord a) => a -> ZTree a -> Bool
member _ E = False
member x (Z a e _ _ izq der)
  | x == a = e
  | x < a = member x izq
  | otherwise = member x der

-- ### d): insertar como en un BST normal. Pero si el elemento ya existía y estaba muerto (zombie), se resucita
insert :: (Ord a) => a -> ZTree a -> ZTree a
insert x E = Z x True 1 0 E E
insert x (Z a e ht hm izq der)
  | x == a && not e = Z a True ht (hm - 1) izq der
  | x <= a = Z a e (size nuevoIzq + size der + 1) (zombies nuevoIzq + zombies der + miEstadoZombie) nuevoIzq der
  | otherwise = Z a e (size izq + size nuevoDer + 1) (zombies izq + zombies nuevoDer + miEstadoZombie) izq nuevoDer
  where
    nuevoIzq = insert x izq
    nuevoDer = insert x der
    miEstadoZombie = if e then 0 else 1

-- ### e): aplanar el árbol en una lista, pero filtrando a los zombies. Con esa lista limpia, armar un nuevo ZTree perfectamente balanceado.
apocalypse :: ZTree a -> ZTree a
apocalypse E = E
apocalypse arbol = crearArbol lista n
  where
    lista = aplanarArbol arbol
    n = length lista

aplanarArbol :: ZTree a -> [a]
aplanarArbol E = []
aplanarArbol (Z a e _ _ izq der)
  | e = aplanarArbol izq ++ [a] ++ aplanarArbol der
  | otherwise = aplanarArbol izq ++ aplanarArbol der

crearArbol :: [a] -> Int -> ZTree a
crearArbol [] _ = E
crearArbol _ 0 = E
crearArbol lista n = Z (head mitad2) True n 0 izq der
  where
    mid = n `div` 2
    mitad1 = take mid lista
    mitad2 = drop mid lista
    izq = crearArbol mitad1 mid
    der = crearArbol (tail mitad2) (n - mid - 1)

-- ### f): buscar el elemento. Si está y estaba vivo, se mata. Atualizar los contadores de zombies del padre. Si al volver al nodo raíz la cantidad de zombies del árbol es mayor a la mitad del tamaño total, aplicar apocalypse.
auxKill :: (Ord a) => a -> ZTree a -> ZTree a
auxKill _ E = E
auxKill x (Z a e ht hm izq der)
  | x == a && e = Z a False ht (hm + 1) izq der
  | x == a && not e = Z a e ht hm izq der
  | x < a = Z a e ht (zombies nuevoIzq + zombies der + miEstadoZombie) nuevoIzq der
  | otherwise = Z a e ht (zombies izq + zombies nuevoDer + miEstadoZombie) izq nuevoDer
  where
    nuevoIzq = auxKill x izq
    nuevoDer = auxKill x der
    miEstadoZombie = if e then 0 else 1

kill :: (Ord a) => a -> ZTree a -> ZTree a
kill _ E = E
kill x arbol =
  if zombies nuevoArbol > size arbol `div` 2
    then apocalypse nuevoArbol
    else nuevoArbol
  where
    nuevoArbol = auxKill x arbol