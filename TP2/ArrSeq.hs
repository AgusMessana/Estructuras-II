module ArrSeq where

import Arr ((!))
import Arr qualified as A
import Par
import Seq

emptyA :: A.Arr a
emptyA = A.empty

singletonA :: a -> A.Arr a
singletonA x = A.fromList [x]

lengthA :: A.Arr a -> Int
lengthA = A.length

nthA :: A.Arr a -> Int -> a
nthA = (!)

takeA :: A.Arr a -> Int -> A.Arr a
takeA arr i
  | i <= 0 = A.empty
  | i <= lengthA arr = A.subArray 0 i arr
  | otherwise = arr

dropA :: A.Arr a -> Int -> A.Arr a
dropA arr i
  | i <= 0 = arr
  | i < n = A.subArray i (n - i) arr
  | otherwise = A.empty
  where
    n = lengthA arr

tabulateA :: (Int -> a) -> Int -> A.Arr a
tabulateA = A.tabulate

mapA :: (a -> b) -> A.Arr a -> A.Arr b
mapA f arr = tabulateA (\i -> f (nthA arr i)) (lengthA arr)

appendA :: A.Arr a -> A.Arr a -> A.Arr a
appendA arr1 arr2 = tabulateA (\i -> if i < l1 then arr1 ! i else arr2 ! (i - l1)) (l1 + l2)
  where
    l1 = lengthA arr1
    l2 = lengthA arr2

filterA :: (a -> Bool) -> A.Arr a -> A.Arr a
filterA p arr =
  let arrOfArrs =
        tabulateA
          ( \i ->
              let x = nthA arr i
               in if p x then singletonA x else emptyA
          )
          (lengthA arr)
   in A.flatten arrOfArrs

showtA :: A.Arr a -> TreeView a (A.Arr a)
showtA arr = case (lengthA arr) of
  0 -> EMPTY
  1 -> ELT (nthA arr 0)
  n -> NODE (takeA arr (n `div` 2)) (dropA arr (n `div` 2))

showlA :: A.Arr a -> ListView a (A.Arr a)
showlA arr = case (lengthA arr) of
  0 -> NIL
  n -> CONS (nthA arr 0) (dropA arr 1)

joinA :: A.Arr (A.Arr a) -> A.Arr a
joinA = A.flatten

fromListA :: [a] -> A.Arr a
fromListA = A.fromList

contrA :: (a -> a -> a) -> A.Arr a -> A.Arr a
contrA f arr = tabulateA faux m
  where
    faux i = if i == (mid) then nthA arr (n - 1) else faux' i
    faux' i = f (nthA arr (i * 2)) (nthA arr (i * 2 + 1))
    mid = n `div` 2
    n = lengthA arr
    m = if n `mod` 2 == 0 then mid else mid + 1

reduceA :: (a -> a -> a) -> a -> A.Arr a -> a
reduceA f e arr = case (lengthA arr) of
  0 -> e
  1 -> f e (nthA arr 0)
  n -> reduceA f e (contrA f arr)

expandA :: (a -> a -> a) -> a -> A.Arr a -> A.Arr a -> A.Arr a
expandA f b v v' = tabulateA aux (lengthA v)
  where
    aux i = case even i of
      True -> nthA v' (div i 2)
      False -> f (nthA v' (div i 2)) (nthA v (i - 1))

scanA :: (a -> a -> a) -> a -> A.Arr a -> (A.Arr a, a)
scanA f b v = case lengthA v of
  0 -> (emptyA, b)
  1 -> (singletonA b, f b (nthA v 0))
  otherwise ->
    let (v', r) = scanA f b (contrA f v)
     in (expandA f b v v', r)

instance Seq A.Arr where
  emptyS = emptyA
  lengthS = lengthA
  nthS = nthA
  singletonS = singletonA
  tabulateS = tabulateA
  mapS = mapA
  filterS = filterA
  appendS = appendA
  dropS = dropA
  takeS = takeA
  showtS = showtA
  showlS = showlA
  reduceS = reduceA
  fromList = fromListA
  joinS = joinA
  scanS = scanA
