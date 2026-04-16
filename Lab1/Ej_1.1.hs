module Lab01 where
import Data.List

{-
1) Corregir los siguientes programas de modo que sean aceptados por GHCi.
-}

-- a)
not' b = case b of
  True -> False
  False -> True

-- b)
init' [x]         =  []
init' (x:xs)      =  x : init' xs
init' []          =  error "empty list"

-- c)
length' []        =  0
length' (_:l)     =  1 + length' l

-- d)
list123 = 1 : (2 : (3 : []))

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = map (++ [x]) (tail xs)

-- g)
listmin xs = head (sort xs)

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs