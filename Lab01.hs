module Lab01 where

import Data.Char
import Data.List

-- =============================================================================
-- # Ejercicio 1: corregir los siguientes programas de modo que sean aceptados por GHCi.
-- =============================================================================

-- ### Inciso a
not' b = case b of
  True -> False
  False -> True

-- ### Inciso b
init' [x] = []
init' (x : xs) = x : init' xs
init' [] = error "empty list"

-- ### Inciso c
length' [] = 0
length' (_ : l) = 1 + length' l

-- ### Inciso d
list123 = 1 : (2 : (3 : []))

-- ### Inciso e
[] ++! ys = ys
(x : xs) ++! ys = x : xs ++! ys

-- ### Inciso f
addToTail x xs = map (++ [x]) (tail xs)

-- ### Inciso g
listmin xs = head (sort xs)

-- ### Inciso h
smap f [] = []
smap f [x] = [f x]
smap f (x : xs) = f x : smap f xs

-- =============================================================================
-- # Ejercicio 2: definir las siguientes funciones y determinar su tipo.
-- =============================================================================

-- ### Inciso a: five, que dado cualquier valor, devuelve 5
five :: a -> Int
five x = 5

-- ### Inciso b: apply, que toma una función y un valor, y devuelve el resultado de aplicar la función al valor dado
apply :: (a -> b) -> a -> b
apply f x = f x

-- ### Inciso c: ident, la función identidad
ident :: a -> a
ident x = x

-- ### Inciso d: first, que toma un par ordenado, y devuelve su primera componente
first :: (a, b) -> a
first (x, y) = x

-- ### Inciso e: derive, que aproxima la derivada de una función dada en un punto dado
derive :: (Fractional a) => (a -> a) -> a -> a
derive f x = (f (x + h) - f x) / h
  where
    h = 0.0001

-- ### Inciso f: sign, la función signo
sign :: (Num a, Ord a) => a -> a
sign x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- ### Inciso g: vabs, la función valor absoluto (usando sign y sin usarla)
vabsConSign :: (Num a, Ord a) => a -> a
vabsConSign x = if sign x >= 0 then x else -x

vabsSinSign :: (Num a, Ord a) => a -> a
vabsSinSign x
  | x >= 0 = x
  | x < 0 = -x

-- ### Inciso h: pot, que toma un entero y un número, y devuelve el resultado de elevar el segundo a la potencia dada por el primero
pot :: (Fractional a) => Int -> a -> a
pot x y = y ^^ x

-- ### Inciso i: xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True = True
xor True False = True
xor True True = False

-- ### Inciso j: max3, que toma tres números enteros y devuelve el máximo entre ellos
max3 :: Int -> Int -> Int -> Int
max3 x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | z >= x && z >= y = z

-- ### Inciso k: swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

{-
================================================================================
# Ejercicio 3: Definir una función que determine si un año es bisiesto o no, de acuerdo a la siguiente definición:
año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
================================================================================
-}
esDiv400 :: Int -> Bool
esDiv400 x = mod x 400 == 0

esDiv100 :: Int -> Bool
esDiv100 x = mod x 100 == 0

esDiv4 :: Int -> Bool
esDiv4 x = mod x 4 == 0

esBisiesto :: Int -> Bool
esBisiesto x
  | esDiv400 x = True
  | esDiv100 x = False
  | esDiv4 x = True
  | otherwise = False

{-
================================================================================
# Ejercicio 4: definir un operador infijo *$ que implemente la multiplicación de un vector por un escalar. Representaremos a los vectores mediante listas de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n debe ser igual a la lista ns con todos sus elementos multiplicados por n.

Por ejemplo,
[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:
v = [1, 2, 3] *$ 2 *$ 4
================================================================================
-}

infixl 7 *$ -- Para que tenga la misma asociatividad y precedencia que la multiplicación

(*$) :: (Num a) => [a] -> a -> [a]
ns *$ n = map (* n) ns

-- =============================================================================
-- # Ejercicio 5: definir las siguientes funciones usando listas por comprensión
-- =============================================================================

-- ### Inciso a: 'divisors', que dado un entero positivo 'x' devuelve la lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
divisors :: Int -> [Int]
divisors x = [y | y <- [1 .. x], x `mod` y == 0]

-- ### Inciso b: 'matches', que dados un entero 'x' y una lista de enteros descarta de la lista los elementos distintos a 'x'
matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, y == x]

-- ### Inciso c: 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2, donde 0 <= a, b, c, d <= 'n'
cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a, b, c, d) | a <- [0 .. n], b <- [0 .. n], c <- [0 .. n], d <- [0 .. n], a ^ 2 + b ^ 2 == c ^ 2 + d ^ 2]

-- ### Inciso d: 'unique', que dada una lista 'xs' de enteros, devuelve la lista 'xs' sin elementos repetidos
unique :: [Int] -> [Int]
unique xs = [x | (x, i) <- zip xs [0 ..], not (elem x (take i xs))]

unique' :: (Eq a) => [a] -> [a]
unique' [] = []
unique' (x : xs) = x : unique' [y | y <- xs, y /= x]

{-
================================================================================
-- # Ejercicio 6: el producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas. Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas..
================================================================================
-}

scalarProduct :: (Num a) => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- =============================================================================
-- # Ejercicio 7: definir mediante recursión explícita las siguientes funciones y escribir su tipo más general:
-- =============================================================================

-- ### Inciso a: 'suma', que suma todos los elementos de una lista de números
suma :: (Num a) => [a] -> a
suma [] = 0
suma (x : xs) = x + suma xs

-- ### Inciso b: 'alguno', que devuelve True si algún elemento de una lista de valores booleanos es True, y False en caso contrario
alguno :: [Bool] -> Bool
alguno [] = False
alguno (x : xs) = x || alguno xs

-- ### Inciso c: 'todos', que devuelve True si todos los elementos de una lista de valores booleanos son True, y False en caso contrario
todos :: [Bool] -> Bool
todos [] = True
todos (x : xs) = x && todos xs

-- ### Inciso d: 'codes', que dada una lista de caracteres, devuelve la lista de sus ordinales
codes :: [Char] -> [Int]
codes [] = []
codes (x : xs) = ord x : codes xs

-- ### Inciso e: 'restos', que calcula la lista de los restos de la división de los elementos de una lista de números dada por otro número dado
restos :: (Integral a) => [a] -> a -> [a]
restos [] _ = []
restos (y : ys) x = mod y x : restos ys x

-- ### Inciso f: 'cuadrados', que dada una lista de números, devuelva la lista de sus cuadrados
cuadrados :: (Num a) => [a] -> [a]
cuadrados [] = []
cuadrados (x : xs) = x ^ 2 : cuadrados xs

-- ### Inciso g: 'longitudes', que dada una lista de listas, devuelve la lista de sus longitudes
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (xs : xss) = length xs : longitudes xss

-- ### Inciso h: 'orden', que dada una lista de pares de números, devuelve la lista de aquellos pares en los que la primera componente es menor que el triple de la segunda
orden :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
orden [] = []
orden ((a, b) : xs) =
  if a < 3 * b
    then (a, b) : orden xs
    else orden xs

-- ### Inciso i: 'pares', que dada una lista de enteros, devuelve la lista de los elementos pares
pares :: [Int] -> [Int]
pares [] = []
pares (x : xs) =
  if even x
    then x : pares xs
    else pares xs

-- ### Inciso j: 'letras', que dada una lista de caracteres, devuelve la lista de aquellos que son letras (minúsculas o mayúsculas)
letras :: [Char] -> [Char]
letras [] = []
letras (x : xs)
  | isAlpha x = x : letras xs
  | otherwise = letras xs

-- ### Inciso k: 'masDe', que dada una lista de listas 'xss' y un número 'n', devuelve la lista de aquellas listas de 'xss' con longitud mayor que 'n'
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (xs : xss) n
  | n < length xs = xs : masDe xss n
  | otherwise = masDe xss n