module Seq where

import Prelude hiding (map, filter, length, take, drop, zip)
import qualified Prelude as P

type Seq a = [a]

emptyS :: Seq a
emptyS = []

singleton :: a -> Seq a
singleton x = [x]

length :: Seq a -> Int
length = P.length

nth :: Seq a -> Int -> a
nth s i = s !! i

tabulate :: (Int -> a) -> Int -> Seq a
tabulate f n = P.map f [0 .. n-1]

map :: (a -> b) -> Seq a -> Seq b
map = P.map

filter :: (a -> Bool) -> Seq a -> Seq a
filter = P.filter

append :: Seq a -> Seq a -> Seq a
append = (++)

take :: Seq a -> Int -> Seq a
take s n = P.take n s

drop :: Seq a -> Int -> Seq a
drop s n = P.drop n s

reduce :: (a -> a -> a) -> a -> Seq a -> a
reduce _ b [] = b
reduce f b s = b `f` foldr1 f s

-- scan exclusivo: la posicion i tiene la reduccion de los primeros i
scan :: (a -> a -> a) -> a -> Seq a -> (Seq a, a)
scan f b s = (P.init acums, P.last acums)
  where acums = P.scanl f b s