module Ej_1_2 where

import Data.List

{-
2) Definir las siguientes funciones y determinar su tipo:
-}

-- a) five, que dado cualquier valor, devuelve 5
five :: a -> Int
five x = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de aplicar la función al valor dado
apply :: (a -> b) -> a -> b
apply f x = f x

-- c) ident, la función identidad
ident :: a -> a
ident x = x

-- d) first, que toma un par ordenado, y devuelve su primera componente
first :: (a, b) -> a
first (x, y) = x

-- e) derive, que aproxima la derivada de una función dada en un punto dado
derive :: (Fractional a) => (a -> a) -> a -> a
derive f x = (f (x + h) - f x) / h
  where
    h = 0.0001

-- f) sign, la función signo
sign :: (Num a, Ord a) => a -> a
sign x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- g) vabs, la función valor absoluto (usando sign y sin usarla)
vabsConSign :: (Num a, Ord a) => a -> a
vabsConSign x = if sign x >= 0 then x else -x

vabsSinSign :: (Num a, Ord a) => a -> a
vabsSinSign x
  | x >= 0 = x
  | x < 0 = -x

-- h) pot, que toma un entero y un número, y devuelve el resultado de elevar el segundo a la potencia dada por el primero
pot :: (Fractional a) => Int -> a -> a
pot x y = y ^^ x

-- i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True = True
xor True False = True
xor True True = False

-- j) max3, que toma tres números enteros y devuelve el máximo entre ellos
max3 :: Int -> Int -> Int -> Int
max3 x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | z >= x && z >= y = z

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)