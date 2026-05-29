module Practica6 where

(|||) :: a -> b -> (a, b)
a ||| b = (a, b)

-- =============================================================================
-- # Ejercicio 1
-- =============================================================================
data BTree a = Empty | Node Int (BTree a) a (BTree a)

nth :: BTree a -> Int -> a
nth Empty _ = error "Índice fuera de rango"
nth (Node t izq x der) n
  | n == size izq = x
  | n < size izq = nth izq n
  | otherwise = nth der (n - size izq - 1)
  where
    size Empty = 0
    size (Node t _ _ _) = t

{-
W(h) = W(h - 1) + O(1) ∈ O(h)
S(h) = S(h - 1) + O(1) ∈ O(h)
-}

cons :: a -> BTree a -> BTree a
cons x Empty = Node 1 Empty x Empty
cons x arbol@(Node t izq y der) = Node (t + 1) Empty x arbol

{-
W(n) ∈ O(1)
S(n) ∈ O(1)
-}

tabulate :: (Int -> a) -> Int -> BTree a
tabulate f 0 = Empty
tabulate f n = worker n 0
  where
    worker 0 _ = Empty
    worker n inicio =
      let mid = n `div` 2
          (izq, der) = worker mid inicio ||| worker (n - mid - 1) (inicio + mid + 1)
       in Node n izq (f (inicio + mid)) der

{-
W(n) = 2*W(floor(n/2)) + O(1) ∈ O(n)
S(n) = S(floor(n/2)) + O(1) ∈ O(lg n)
-}

mapTree :: (a -> b) -> BTree a -> BTree b
mapTree f Empty = Empty
mapTree f (Node t izq x der) =
  let (izq', der') = mapTree f izq ||| mapTree f der
   in Node t izq' (f x) der'

{-
W(n) = 2*W(floor(n/2)) + O(1) ∈ O(n)
S(n) = W(floor(n/2)) + O(1) ∈ O(lg n)
-}

take' :: Int -> BTree a -> BTree a
take' _ Empty = Empty
take' 0 _ = Empty
take' k arbol@(Node t izq x der)
  | k >= t = arbol
  | k <= size izq = take' k izq
  | otherwise = Node k izq x (take' (k - size izq - 1) der)
  where
    size Empty = 0
    size (Node t _ _ _) = t

{-
W(h) = W(h - 1) + O(1) ∈ O(h)
S(h) = S(h - 1) + O(1) ∈ O(h)
-}

drop' :: Int -> BTree a -> BTree a
drop' _ Empty = Empty
drop' 0 arbol = arbol
drop' k arbol@(Node t izq x der)
  | k >= t = Empty
  | k <= size izq = Node (t - k) (drop' k izq) x der
  | otherwise = drop' (k - size izq - 1) der
  where
    size Empty = 0
    size (Node t _ _ _) = t

{-
W(h) = W(h - 1) + O(1) ∈ O(h)
S(h) = S(h - 1) + O(1) ∈ O(h)
-}

-- =============================================================================
-- Ejercicio 2
-- =============================================================================
data Tree a = E | Leaf a | Join (Tree a) (Tree a)

-- ### Inciso a)
mcss :: (Num a, Ord a) => Tree a -> a
mcss arbol = m
  where
    (m, p, s, tot) = mapReduce tuplaHoja combinarTuplas (0, 0, 0, 0) arbol

combinarTuplas :: (Num a, Ord a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
combinarTuplas (ml, pl, sl, tl) (mr, pr, sr, tr) = (mcssNuevo, prefijoNuevo, sufijoNuevo, totalNuevo)
  where
    totalNuevo = tl + tr
    prefijoNuevo = max pl (tl + pr)
    sufijoNuevo = max sr (tr + sl)
    mcssNuevo = max ml (max mr (sl + pr))

tuplaHoja :: (Num a, Ord a) => a -> (a, a, a, a)
tuplaHoja v = (max v 0, max v 0, max v 0, v)

mapReduce :: (a -> b) -> (b -> b -> b) -> b -> Tree a -> b
mapReduce f c base E = base
mapReduce f c base (Leaf v) = f v
mapReduce f c base (Join izq der) = c izq' der'
  where
    (izq', der') = mapReduce f c base izq ||| mapReduce f c base der

-- ### Inciso b)
{-
W(n) = W(n_izq) + W(n_der) + O(1) ∈ O(n)
S(n) = máx{S(h_izq), S(h_der)} + O(1) ∈ O(h)
-}

-- =============================================================================
-- # Ejercicio 3
-- =============================================================================
mejorGanancia :: Tree Int -> Int
mejorGanancia arbol = maxAll arbolDeGanancias
  where
    pares = conSufijos arbol
    arbolDeGanancias = mapTree' calcularGanancias pares

calcularGanancias :: (Int, Tree Int) -> Tree Int
calcularGanancias (compra, arbolVentas) = mapTree' (\venta -> venta - compra) arbolVentas

mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f E = E
mapTree' f (Leaf v) = Leaf (f v)
mapTree' f (Join izq der) =
  let (izq', der') = mapTree' f izq ||| mapTree' f der
   in Join izq' der'

sufijos :: Tree Int -> Tree (Tree Int)
sufijos E = E
sufijos (Leaf _) = Leaf E
sufijos (Join izq der) = Join nuevoIzq (sufijos der)
  where
    nuevoIzq = mapTree' (\t -> Join t der) (sufijos izq)

juntar :: Tree Int -> Tree (Tree Int) -> Tree (Int, Tree Int)
juntar E E = E
juntar (Leaf v) (Leaf s) = Leaf (v, s)
juntar (Join izq1 der1) (Join izq2 der2) = Join (juntar izq1 izq2) (juntar der1 der2)

conSufijos :: Tree Int -> Tree (Int, Tree Int)
conSufijos E = E
conSufijos (Leaf v) = Leaf (v, E)

maxT :: Tree Int -> Int
maxT = reduce max minBound

reduce :: (a -> a -> a) -> a -> Tree a -> a
reduce c base E = base
reduce c base (Leaf v) = v
reduce c base (Join izq der) = c izq' der'
  where
    (izq', der') = reduce c base izq ||| reduce c base der

maxAll :: Tree (Tree Int) -> Int
maxAll = mapReduce maxT max minBound

-- =============================================================================
-- # Ejercicio 4
-- =============================================================================
data T a = Em | N (T a) a (T a)

altura :: T a -> Int
altura Em = 0
altura (N l x r) = 1 + max (altura l) (altura r)

-- ### Inciso a)
combinar :: T a -> T a -> T a
combinar Em t2 = t2
combinar (N l x r) t2 = N (combinar l r) x t2

-- ### Inciso b)
filterT :: (a -> Bool) -> T a -> T a
filterT p Em = Em
filterT p (N l x r)
  | p x = N l' x r'
  | otherwise = combinar l' r'
  where
    (l', r') = filterT p l ||| filterT p r

-- ### Inciso c
quicksortT :: T Int -> T Int
quicksortT Em = Em
quicksortT (N l x r) = N menores x mayores
  where
    resto = combinar l r
    (menores, mayores) =
      quicksortT (filterT (<= x) resto) ||| quicksortT (filterT (> x) resto)

{-
Para el peor caso, donde el árbol está totalmente desbalanceado, es decir, cuando d = n, vemos lo siguiente:
Sabemos que el costo de la función filter es SfilterT(d) ∈ O(d^2) = O(n^2).
Para la ecucaciónde recurrencia, tenemos que sumar el costo de la llamada recursiva más el costo de la partición actual. O sea:
W(n) = W(n - 1) + n^2.
Por lo tanto, concluímos que W(n) ∈ O(n^3).

En el caso de estar perfectamente balanceado, tomando n la cantidad de nodos, tenemos que d = lg n.
Para este caso, como sabemos que altura (filterT p t) <= altura t y en este caso d = lg n, el costo de aplicar filterT es (lg n)^2. Luego:
S(n) = S(floor(n/2)) + (lg n)^2 ∈ O((lg n)^3)

Por otro lado, el trabajo de chequear que cada elemento sea menor o mayor que x hace que la función tenga que pasar por todos los elementos. Luego, el trabajo de filtrado es O(n).
Luego, W(n) = 2W(floor(n/2)) + n ∈ O(n lg n)

Caso 1 a 9 o 1 a 99
Cuando el pivote es desbalanceado y divide los datos en proporciones asimétricas (por ejemplo, enviando el 90% o el 99% a una de las ramas), el tamaño del problema en la rama más pesada decrece multiplicándose por una fracción constante (9/10 o 99/100). Esto genera un árbol de llamadas cuya altura está dada por un logaritmo con base distinta a 2, en estos casos puede ser en base 10/9 o 100/99.
Por las propiedades de cambio de base, cualquier logaritmo difiere de otro únicamente por una constante multiplicativa. Como la notación O ignora esas constantes, la altura máxima de recursión sigue siendo O(log n)
Aunque en la práctica la constante oculta haga que el algoritmo ejecute más pasos, a nivel teórico el orden de complejidad se mantiene intacto respecto al mejor caso. Para cualquier proporción constante (1 a 9 o 1 a 99), el Trabajo total será W ∈ O(n lg n) y la Profundidad será S ∈ O(log^3 n).
-}

-- =============================================================================
-- Ejercicio 5
-- =============================================================================
