module Practica4 where

-- =============================================================================
-- # Ejercicio 1: fefinir las siguientes funciones de manera que se puedan compartir la mayor cantidad posible de elementos de los árboles creados.
-- =============================================================================
data Tree a = Leaf | Node (Tree a) a (Tree a)

-- ### Inciso a): dado un valor x de tipo a y un entero d, crea un árbol binario completo de altura d con el valor x en cada nodo.
completo :: a -> Int -> Tree a
completo x 0 = Leaf
completo x d = Node t x t
  where
    t = completo x (d - 1)

-- ### Inciso b): dado un valor x de tipo a y un entero n, crea un árbol binario balanceado de tamaño n, con el valor x en cada nodo.
balanceado :: a -> Int -> Tree a
balanceado x 0 = Leaf
balanceado x n
  | odd n = Node t x t
  | otherwise = Node ti x td
  where
    m = (n - 1) `div` 2
    t = balanceado x m
    ti = balanceado x (m + 1)
    td = balanceado x m

-- =============================================================================
-- # Ejercicio 2: definir las siguientes funciones sobre árboles binarios de búsqueda (bst)
-- =============================================================================
data BST a = Hoja | Nodo (BST a) a (BST a)

-- ### Inciso a): calcular el máximo valor en un bst.
maximum' :: (Ord a) => BST a -> a
maximum' (Nodo _ x Hoja) = x
maximum' (Nodo _ _ der) = maximum' der

-- ### Inciso b): chequear si un árbol binario es un bst.
checkBST :: (Ord a) => BST a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja x Hoja) = True
checkBST (Nodo Hoja x der) = x < minimum' der && checkBST der
checkBST (Nodo izq x Hoja) = maximum' izq <= x && checkBST izq
checkBST (Nodo izq x der) = maximum' izq <= x && minimum' der > x && checkBST izq && checkBST der

minimum' :: (Ord a) => BST a -> a
minimum' (Nodo Hoja x _) = x
minimum' (Nodo izq _ _) = minimum' izq

-- ### Inciso c): dado un árbol bst t y un elemento x , devuelva una tupla con un bst con los elementos de t menores o iguales a x y un bst con los elementos de t mayores a x .
splitBST :: (Ord a) => BST a -> a -> (BST a, BST a)
splitBST Hoja x = (Hoja, Hoja)
splitBST (Nodo izq y der) x
  | x == y = (Nodo izq y Hoja, der)
  | x < y =
      let (izq', der') = splitBST izq x
       in (izq', Nodo der' y der)
  | x > y =
      let (izq', der') = splitBST der x
       in (Nodo izq y izq', der')

-- ### Inciso d): unir los elementos dos árboles bst en uno.
join :: (Ord a) => BST a -> BST a -> BST a
join a1 Hoja = a1
join Hoja a2 = a2
join (Nodo izq y der) a2 =
  let (menores_a2, mayores_a2) = splitBST a2 y
   in Nodo (join izq menores_a2) y (join der mayores_a2)

-- =============================================================================
-- # Ejercicio 3: definir member en términos de una función auxiliar que tenga como parámetro el elemento candidato, el cual puede ser igual al elemento que se desea buscar y que chequee que los elementos son iguales sólo cuando llega a una hoja del árbol.

-- arbol -> lo que busco -> aproximacion
-- =============================================================================
auxiliarMember :: (Ord a) => BST a -> a -> Maybe a -> Bool
auxiliarMember Hoja x Nothing = False
auxiliarMember Hoja x (Just y) = x == y
auxiliarMember (Nodo izq v der) x y
  | x < v = auxiliarMember izq x y
  | otherwise = auxiliarMember der x (Just v)

member' :: (Ord a) => BST a -> a -> Bool
member' arbol x = auxiliarMember arbol x Nothing

-- =============================================================================
-- # Ejercicio 4
-- =============================================================================
data Color = R | B

data RBT a = E | T Color (RBT a) a (RBT a)

-- ### Inciso a): definir dos funciones lbalance y rbalance que chequeen que el invariante 1 se cumple en los subárboles izquierdo y derecho respectivamente.

lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r

-- ### Inciso b): reemplazar las llamadas a balance en ins por llamadas a alguna de estas dos funciones.
insert :: (Ord a) => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
  where
    ins x E = T R E x E
    ins x (T c l y r)
      | x < y = lbalance c (ins x l) y r
      | x > y = rbalance c l y (ins x r)
      | otherwise = T c l y r
    makeBlack E = E
    makeBlack (T _ l x r) = T B l x r

-- =============================================================================
-- # Ejercicio 5:
-- =============================================================================

-- con paralelizacion dedinir un operador de paralelelismo a ||| b = (a, b)