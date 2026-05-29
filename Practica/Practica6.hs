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