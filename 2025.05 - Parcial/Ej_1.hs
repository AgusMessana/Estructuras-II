module Ej_1 where

-- =============================================================================
-- # Ejercicio 1: definir eficientemente las siguientes funciones en Haskell.
-- =============================================================================

data Scapegoat a = E | N a Int (Scapegoat a) (Scapegoat a) deriving (Eq, Show)

-- ### Inciso a): calcular el tamaño del árbol (número de nodos).
size :: Scapegoat a -> Int
size E = 0
size (N _ x _ _) = x

--- ### Inciso b): dado un valor de tipo Scapegoat a determine si es un árbol binario de búsqueda.
isBST :: (Ord a) => Scapegoat a -> Bool
isBST E = True
isBST arbol = auxIsBST arbol Nothing Nothing

auxIsBST :: (Ord a) => Scapegoat a -> Maybe a -> Maybe a -> Bool
auxIsBST E _ _ = True
auxIsBST (N a x izq der) piso techo = respetarPiso a piso && respetarTecho a techo && auxIsBST izq piso (Just a) && auxIsBST der (Just a) techo

respetarPiso :: (Ord a) => a -> Maybe a -> Bool
respetarPiso _ Nothing = True
respetarPiso valor (Just p) = valor >= p

respetarTecho :: (Ord a) => a -> Maybe a -> Bool
respetarTecho _ Nothing = True
respetarTecho valor (Just t) = valor < t

-- ### Inciso c): dado un valor de tipo Scapegoat a determina si es un Scapegoat tree.
isScapegoatTree :: (Ord a) => Scapegoat a -> Bool
isScapegoatTree E = True
isScapegoatTree arbol = isBST arbol && cumplePesos arbol
  where
    cumplePesos E = True
    cumplePesos tree@(N _ _ izq der) =
      3 * size izq <= 2 * size tree
        && 3 * size der <= 2 * size tree
        && cumplePesos izq
        && cumplePesos der

-- ### Inciso d): dado un Scapegoat tree y un elemento determine si el elemento está en el árbol
member :: (Ord a) => a -> Scapegoat a -> Bool
member _ E = False
member y (N a x izq der)
  | y == a = True
  | y < a = member y izq
  | otherwise = member y der

-- ### Inciso e): dado un valor de tipo Scapegoat a lo reestructure a otro árbol binario de búsqueda con exactamente los mismos elementos pero casi perfectamente balanceado.
listaInOrder :: Scapegoat a -> [a]
listaInOrder E = []
listaInOrder (N a x izq der) = listaInOrder izq ++ [a] ++ listaInOrder der

midLength :: [a] -> Int
midLength lista = length lista `div` 2

armarArbol :: [a] -> Scapegoat a
armarArbol [] = E
armarArbol lista = N r l (armarArbol izq) (armarArbol der)
  where
    l = length lista
    mid = midLength lista
    izq = take mid lista
    (r : der) = drop mid lista

rebuild :: Scapegoat a -> Scapegoat a
rebuild E = E
rebuild arbol = armarArbol (listaInOrder arbol)

-- ### Inciso f): insertar un elemento en un Scapegoat tree, preservando el invariante del balanceo.
insert :: (Ord a) => a -> Scapegoat a -> Scapegoat a
insert a E = N a 1 E E
insert x (N a s izq der)
  | x <= a =
      let nuevoSubIzq = N a (s + 1) (insert x izq) der
       in if 3 * (size izq + 1) <= 2 * (s + 1)
            then nuevoSubIzq
            else rebuild nuevoSubIzq
  | otherwise =
      let nuevoSubDer = N a (s + 1) izq (insert x der)
       in if 3 * (size der + 1) <= 2 * (s + 1)
            then nuevoSubDer
            else rebuild nuevoSubDer