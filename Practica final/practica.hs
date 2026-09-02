data Tree a = E | N Int (Tree a) (Tree a) | L a deriving (Show)

(|||) :: a -> b -> (a, b)
a ||| b = (a, b)

toList :: Tree a -> [a]
toList E = []
toList (L x) = [x]
toList (N _ l r) = toList l ++ toList r

t5 :: Tree Int
t5 = N 5 (N 2 (L 0) (L 1)) (N 3 (L 2) (N 2 (L 3) (L 4)))

t1 :: Tree Int
t1 = L 7

t0 :: Tree Int
t0 = E

t4 :: Tree Int
t4 = N 4 (N 2 (L 10) (L 20)) (N 2 (L 30) (L 40))

tD :: Tree Int
tD = N 4 (L 1) (N 3 (L 2) (N 2 (L 3) (L 4)))

tI :: Tree Int
tI = N 4 (N 3 (N 2 (L 1) (L 2)) (L 3)) (L 4)
-- =============================================================================
-- # a)
divide :: Tree a -> Int -> (Tree a, Tree a)
divide E _ = (E, E)
divide arbol 0 = (E, arbol)
divide (L x) n = (L x, E)
divide (N _ l r) n
  | n <= size l = let (a, b) = divide l n in (a, join b r)
  | otherwise = let (a, b) = divide r (n - size l) in (join l a, b)

size :: Tree a -> Int
size E = 0
size (L _) = 1
size (N t _ _) = t

join :: Tree a -> Tree a -> Tree a
join t1 E = t1
join E t2 = t2
join t1 t2 = N (size t1 + size t2) t1 t2

-- b)
interleave :: Tree a -> Tree a -> Tree a
interleave s1 E = s1
interleave E s2 = s2
interleave (N t1 l1 r1) (N t2 l2 r2) =
  let (l', r') = interleave l1 l2 ||| interleave r1 r2
   in N (t1 + t2) l' r'
