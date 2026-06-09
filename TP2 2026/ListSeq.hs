module ListSeq where

import Par
import Seq

emptyL :: [a]
emptyL = []

singletonL :: a -> [a]
singletonL x = [x]

lengthL :: [a] -> Int
lengthL [] = 0
lengthL (x : xs) = 1 + lengthL xs

nthL :: [a] -> Int -> a
nthL [] _ = error "Index out of range"
nthL l 0 = head l
nthL l n = nthL (tail l) (n - 1)

tabulateL :: (Int -> a) -> Int -> [a]
tabulateL f n = mapL f [0 .. n - 1]

mapL :: (a -> b) -> [a] -> [b]
mapL _ [] = []
mapL f (x : xs) =
  let (y, ys) = f x ||| mapL f xs
   in y : ys

filterL :: (a -> Bool) -> [a] -> [a] -- PARALELIZO LA p
filterL p [] = []
filterL p (x : xs) =
  let (px, xs') = p x ||| filterL p xs
   in if px then x : xs' else xs'

appendL :: [a] -> [a] -> [a]
appendL l [] = l
appendL [] l = l
appendL (x : xs) l = x : (appendL xs l)

dropL :: [a] -> Int -> [a]
dropL [] n = []
dropL l 0 = l
dropL (x : xs) n = dropL xs (n - 1)

takeL :: [a] -> Int -> [a]
takeL [] n = []
takeL l 0 = []
takeL (x : xs) n = x : (takeL xs (n - 1))

showtL :: [a] -> TreeView a [a]
showtL [] = EMPTY
showtL [a] = ELT a
showtL l = NODE (takeL l (n `div` 2)) (dropL l (n `div` 2)) where n = lengthL l

showlL :: [a] -> ListView a [a]
showlL [] = NIL
showlL (x : xs) = CONS x xs

contrL :: (a -> a -> a) -> [a] -> [a]
contrL f [] = []
contrL f [x] = [x]
contrL f (x1 : x2 : l) =
  let (y, ys) = (f x1 x2) ||| (contrL f l)
   in y : ys

reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL f e [] = e
reduceL f e [x] = f e x
reduceL f e l = reduceL f e (contrL f l)

fromListL :: [a] -> [a]
fromListL l = l

-- Es un foldr
foldAux :: (a -> b -> b) -> b -> [a] -> b
foldAux _ e [] = e
foldAux f e (x : xs) = f x (foldAux f e xs)

-- Usamos foldAux en vez de reduce para que sea O(n) en vez de O(n*lg(n))
joinL :: [[a]] -> [a]
joinL = foldAux appendL []

expandL :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
expandL f b [] _ = []
expandL f b [x] [y] = [y]
expandL f b (x : y : xs) (z : ys) =
  let (sum, recursion) = f z x ||| expandL f b xs ys
   in z : sum : recursion

scanL :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanL f b [] = ([], b)
scanL f b [x] = ([b], f b x)
scanL f b s =
  let (s', r) = scanL f b (contrL f s)
   in (expandL f b s s', r)

instance Seq [] where
  emptyS = emptyL
  lengthS = lengthL
  nthS = nthL
  singletonS = singletonL
  tabulateS = tabulateL
  mapS = mapL
  filterS = filterL
  appendS = appendL
  dropS = dropL
  takeS = takeL
  showtS = showtL
  showlS = showlL
  reduceS = reduceL
  fromList = fromListL
  joinS = joinL
  scanS = scanL
