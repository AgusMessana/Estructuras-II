module Lab02 where

import Data.List

-- =============================================================================
-- # Ejercicio 1: dada la siguiente definición para representar árboles binarios definir las siguientes funciones:
-- =============================================================================

data BTree a = E | Leaf a | Node (BTree a) (BTree a) deriving (Show)

-- ### Inciso a): altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E = 0
altura (Leaf a) = 1
altura (Node izq der) = 1 + max (altura izq) (altura der)

-- ### Inciso b): perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si cada nodo tiene 0 o 2 hijos y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E = True
perfecto (Leaf a) = True
perfecto (Node izq der) = perfecto izq && perfecto der && (altura izq == altura der)

-- ### Inciso c): inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E = []
inorder (Leaf a) = [a]
inorder (Node izq der) = inorder izq ++ inorder der

-- =============================================================================
-- Ejercicio 2: dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):
-- =============================================================================

data GTree a = EG | NodeG a [GTree a]

data BinTree a = EB | NodeB (BinTree a) a (BinTree a)

{-
### Inciso a): definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
La función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB), donde el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).

Por ejemplo, sea t:

        A
     / | | \
    B  C D  E
   /|\     / \
  F G H   I   J
 /\       |
K  L      M

g2bt t =

      A
     /
    B
   / \
  F   C
 / \   \
K   G   D
 \   \   \
  L   H   E
         /
        I
       / \
      M   J
-}

procesarHermanos :: [GTree a] -> BinTree a
procesarHermanos [] = EB
procesarHermanos (NodeG dato hijos : restoHermanos) = NodeB (procesarHermanos hijos) dato (procesarHermanos restoHermanos)

g2bt :: GTree a -> BinTree a
g2bt EG = EB
g2bt (NodeG dato hijos) = NodeB (procesarHermanos hijos) dato EB

-- =============================================================================
-- # Ejercicio 3: utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones:
-- =============================================================================

-- data BinTree a = EB | NodeB (BinTree a) a (BinTree a)

{-
### Inciso a): dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo que contenga la máxima cantidad de elementos posibles.
Por ejemplo, sea t:
   1
 /   \
2     3
 \   / \
  4 5   6
dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
-}

dcn :: BinTree a -> [a]
dcn EB = []
dcn arbol = buscarMayor (reverse (zip [0 ..] (porNiveles arbol)))

buscarMayor :: [(Int, [a])] -> [a]
buscarMayor [] = []
buscarMayor ((nivel, nodos) : resto)
  | 2 ^ nivel == length nodos = nodos
  | otherwise = buscarMayor resto

porNiveles :: BinTree a -> [[a]]
porNiveles EB = []
porNiveles (NodeB izq dato der) = [dato] : juntarNiveles (porNiveles izq) (porNiveles der)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles [] ys = ys
juntarNiveles xs [] = xs
juntarNiveles (x : xs) (y : ys) = (x ++ y) : juntarNiveles xs ys

{-
### Inciso b): maxn, que dado un árbol devuelva la profundidad del nivel completo más profundo.
Por ejemplo, maxn t = 2
-}

maxn :: BinTree a -> Int
maxn = undefined

{-
### Inciso c): podar, que elimine todas las ramas necesarias para transformar el árbol en un árbol completo con la máxima altura posible.
Por ejemplo, podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar :: BinTree a -> BinTree a
podar = undefined
