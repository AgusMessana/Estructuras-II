module Practica4 where

import Text.XHtml (h2)

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

-- ### Inciso 1): definir un tipo de datos que represente árboles 1-2-3.
data Arbol123 a
  = Hoja123
  | Node2 (Arbol123 a) a (Arbol123 a)
  | Node3 (Arbol123 a) a (Arbol123 a) a (Arbol123 a)
  | Node4 (Arbol123 a) a (Arbol123 a) a (Arbol123 a) a (Arbol123 a)

--- ### Inciso 2): definir una función que transforme red-black trees en árboles 1-2-3. Paralelizar cuando sea posible.
(|||) :: a -> b -> (a, b)
x ||| y = (x, y)

convertir :: RBT a -> Arbol123 a
convertir E = Hoja123
convertir (T B (T R izq1 x der1) y (T R izq2 z der2)) =
  let (ni1, nd1) = convertir izq1 ||| convertir der1
      (ni2, nd2) = convertir izq2 ||| convertir der2
   in Node4 ni1 x nd1 y ni2 z nd2
convertir (T B (T R izq1 x der1) y der) =
  let (ni1, nd1) = convertir izq1 ||| convertir der1
      nuevoDer = convertir der
   in Node3 ni1 x nd1 y nuevoDer
convertir (T B izq x (T R izq2 y der2)) =
  let (ni2, nd2) = convertir izq2 ||| convertir der2
      nuevoIzq = convertir izq
   in Node3 nuevoIzq x ni2 y nd2
convertir (T B izq a der) =
  let (nuevoIzq, nuevoDer) = convertir izq ||| convertir der
   in Node2 nuevoIzq a nuevoDer

-- =============================================================================
-- # Ejercicio 6: definir una función que cree un leftist heap a partir de una lista, convirtiendo cada elemento de la lista en un heap de un solo elemento y aplicando la función merge hasta obtener un solo heap. Aplicar la función merge n veces, donde n es la longitud de la lista que recibe como argumento la función.
-- =============================================================================

data LeftistHeap a = LHoja | LNodo Int (LeftistHeap a) a (LeftistHeap a)

fromList :: (Ord a) => [a] -> LeftistHeap a
fromList [] = LHoja
fromList (x : xs) = insert' x (fromList xs)

rank :: LeftistHeap a -> Int
rank LHoja = 0
rank (LNodo r izq a der) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x h1 h2 =
  if rank h1 >= rank h2
    then LNodo newRank1 h1 x h2
    else LNodo newRank2 h2 x h1
  where
    newRank1 = 1 + rank h2
    newRank2 = 1 + rank h1

merge' :: (Ord a) => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge' heap LHoja = heap
merge' LHoja heap = heap
merge' h1@(LNodo r1 izq1 x der1) h2@(LNodo r2 izq2 y der2)
  | x <= y = makeT x izq1 (merge' der1 h2)
  | otherwise = makeT y izq2 (merge' der2 h1)

insert' :: (Ord a) => a -> LeftistHeap a -> LeftistHeap a
insert' x heap = merge' (LNodo 1 LHoja x LHoja) heap

-- =============================================================================
-- # Ejercicio 7: definir las siguientes funciones para un pairing heap
-- =============================================================================

data PHeaps a = Empty | Root a [PHeaps a]

-- ### Inciso a): determinar si un árbol es un pairing heap, es decir, cumple con el invariante de heap.
isPHeap :: (Ord a) => PHeaps a -> Bool
isPHeap Empty = True
isPHeap (Root x hijos) =
  all check hijos && all isPHeap hijos
  where
    check Empty = True
    check (Root y _) = x <= y

-- ### Inciso b): unir dos pairing heap. Para ello, comparar las raíces de ambos árboles y elegir la menor como raíz del nuevo heap, agregar el árbol con mayor raíz como hijo de éste.
merge :: (Ord a) => PHeaps a -> PHeaps a -> PHeaps a
merge Empty heap = heap
merge heap Empty = heap
merge h1@(Root x hijos1) h2@(Root y hijos2)
  | x <= y = Root x (h2 : hijos1)
  | otherwise = Root y (h1 : hijos2)

-- ### Inciso c): insertar un elemento en un pairing heap.
insert'' :: (Ord a) => PHeaps a -> a -> PHeaps a
insert'' Empty x = Root x []
insert'' heap x = merge (Root x []) heap

-- ### Inciso d): dada una lista de pairing heaps, construir otro con los elementos del mismo.
concatHeaps :: (Ord a) => [PHeaps a] -> PHeaps a
concatHeaps listaDeHeaps = foldr merge Empty listaDeHeaps

-- ### Inciso e): dado un pairing heap, devolver, si el árbol no es vacío, un par con el menor elemento y un pairing heap sin éste elemento, o Nothing en otro caso.
delMin :: (Ord a) => PHeaps a -> Maybe (a, PHeaps a)
delMin Empty = Nothing
delMin (Root x hijos) = Just (x, concatHeaps hijos)