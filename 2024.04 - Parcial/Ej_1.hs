module Ej_1 where

-- =============================================================================
-- # Ejercicio 2: definir las siguientes funciones.
-- =============================================================================
data Color = R | B deriving (Eq, Show)

data AAtree a = N Color a (AAtree a) (AAtree a) | E deriving (Eq, Show)

-- ### Inciso a): dado un valor de tipo AAtree a determine si es un árbol binario de búsqueda
isBST :: (Ord a) => AAtree a -> Bool
isBST E = True
isBST arbol = auxIsBST arbol Nothing Nothing

auxIsBST :: (Ord a) => AAtree a -> Maybe a -> Maybe a -> Bool
auxIsBST E _ _ = True
auxIsBST (N _ v izq der) piso techo = checkPiso v piso && checkTecho v techo && auxIsBST izq piso (Just v) && auxIsBST der (Just v) techo

checkPiso :: (Ord a) => a -> Maybe a -> Bool
checkPiso _ Nothing = True
checkPiso v (Just p) = v >= p

checkTecho :: (Ord a) => a -> Maybe a -> Bool
checkTecho _ Nothing = True
checkTecho v (Just t) = v < t

-- ### Inciso b): dado un valor de tipo AAtree a determine si es un AAtree
isAAtree :: (Ord a) => AAtree a -> Bool
isAAtree E = True
isAAtree arbol = isBST arbol && color arbol == B && checkChoqueR arbol && checkAltN arbol /= -1

color :: AAtree a -> Color
color E = B
color (N c _ _ _) = c

checkChoqueR :: AAtree a -> Bool
checkChoqueR E = True
checkChoqueR (N B _ izq der) = color izq /= R && checkChoqueR izq && checkChoqueR der
checkChoqueR (N R _ izq der) = color izq /= R && color der /= R && checkChoqueR izq && checkChoqueR der

checkAltN :: AAtree a -> Int
checkAltN E = 1
checkAltN (N c _ izq der)
  | altIzq == -1 || altDer == -1 || altIzq /= altDer = -1
  | c == B = 1 + altIzq
  | c == R = altIzq
  where
    altIzq = checkAltN izq
    altDer = checkAltN der

-- ### Inciso c): dado un AAtree y un elemento determine si el elemento está en el árbol
member :: (Ord a) => a -> AAtree a -> Bool
member _ E = False
member x (N _ v izq der)
  | x == v = True
  | x < v = member x izq
  | otherwise = member x der

-- ### Inciso d): inserta un elemento en un AAtree. El nuevo árbol debe verificar todas las invariantes para ser AAtree.
insert :: (Ord a) => a -> AAtree a -> AAtree a
insert v arbol = cambiarColor (auxInsert v arbol)

auxInsert :: (Ord a) => a -> AAtree a -> AAtree a
auxInsert v E = N R v E E
auxInsert v (N c x izq der)
  | v <= x = split (skew (N c x (auxInsert v izq) der))
  | otherwise = split (skew (N c x izq (auxInsert v der)))

cambiarColor :: AAtree a -> AAtree a
cambiarColor E = E
cambiarColor (N _ v izq der) = N B v izq der

skew :: AAtree a -> AAtree a
skew E = E
skew (N c y (N R x tA tB) tC) = N c x tA (N R y tB tC)
skew arbol = arbol

split :: AAtree a -> AAtree a
split E = E
split (N c x tA (N R y tB (N R z tC tD))) = N R y (N B x tA tB) (N B z tC tD)
split arbol = arbol