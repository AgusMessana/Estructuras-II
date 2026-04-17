module Ej_1_5 where

import Data.List

{-
5) Definir las siguientes funciones usando listas por comprensión:
-}

-- a) 'divisors', que dado un entero positivo 'x' devuelve la lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
divisors :: Int -> [Int]
divisors x = [y | y <- [1 .. x], x `mod` y == 0]

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta de la lista los elementos distintos a 'x'
matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

-- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2, donde 0 <= a, b, c, d <= 'n'
cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a, b, c, d) | a <- [0 .. n], b <- [0 .. n], c <- [0 .. n], d <- [0 .. n], a ^ 2 + b ^ 2 == c ^ 2 + d ^ 2]

-- (d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista 'xs' sin elementos repetidos
unique :: [Int] -> [Int]
unique xs = [x | (x, i) <- zip xs [0 ..], not (elem x (take i xs))]

unique' :: (Eq a) => [a] -> [a]
unique' [] = []
unique' (x : xs) = x : unique' [y | y <- xs, y /= x]