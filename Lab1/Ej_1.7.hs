module Ej_1_7 where

import Data.Char

{-
7) Definir mediante recursión explícita las siguientes funciones y escribir su tipo más general:
-}

-- a) 'suma', que suma todos los elementos de una lista de números
suma :: (Num a) => [a] -> a
suma [] = 0
suma (x : xs) = x + suma xs

-- b) 'alguno', que devuelve True si algún elemento de una lista de valores booleanos es True, y False en caso contrario
alguno :: [Bool] -> Bool
alguno [] = False
alguno (x : xs) = x || alguno xs

-- c) 'todos', que devuelve True si todos los elementos de una lista de valores booleanos son True, y False en caso contrario
todos :: [Bool] -> Bool
todos [] = True
todos (x : xs) = x && todos xs

-- d) 'codes', que dada una lista de caracteres, devuelve la lista de sus ordinales
codes :: [Char] -> [Int]
codes [] = []
codes (x : xs) = ord x : codes xs

-- e) 'restos', que calcula la lista de los restos de la división de los elementos de una lista de números dada por otro número dado
restos :: (Integral a) => [a] -> a -> [a]
restos [] _ = []
restos (y : ys) x = mod y x : restos ys x

-- f) 'cuadrados', que dada una lista de números, devuelva la lista de sus cuadrados
cuadrados :: (Num a) => [a] -> [a]
cuadrados [] = []
cuadrados (x : xs) = x ^ 2 : cuadrados xs

-- g) 'longitudes', que dada una lista de listas, devuelve la lista de sus longitudes
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (xs : xss) = length xs : longitudes xss

-- h) 'orden', que dada una lista de pares de números, devuelve la lista de aquellos pares en los que la primera componente es menor que el triple de la segunda
orden :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
orden [] = []
orden ((a, b) : xs) =
  if a < 3 * b
    then (a, b) : orden xs
    else orden xs

-- i) 'pares', que dada una lista de enteros, devuelve la lista de los elementos pares
pares :: [Int] -> [Int]
pares [] = []
pares (x : xs) =
  if even x
    then x : pares xs
    else pares xs

-- j) 'letras', que dada una lista de caracteres, devuelve la lista de aquellos que son letras (minúsculas o mayúsculas)
letras :: [Char] -> [Char]
letras [] = []
letras (x : xs)
  | isAlpha x = x : letras xs
  | otherwise = letras xs

-- k) 'masDe', que dada una lista de listas 'xss' y un número 'n', devuelve la lista de aquellas listas de 'xss' con longitud mayor que 'n'
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (xs : xss) n
  | n < length xs = xs : masDe xss n
  | otherwise = masDe xss n