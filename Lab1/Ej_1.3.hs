module Ej_1_3 where

import Data.List

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
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