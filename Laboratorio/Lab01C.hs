module Lab01C where

import Data.List
import Data.Ord
import Data.Text (Text)

type Texto = String

-- =============================================================================
-- # Ejercicio 1: definir una función que dado un caracter y un texto calcule la frecuencia con la que ocurre el caracter en el texto.
-- Por ejemplo: frecuency 'a' "casa" = 0.5
-- =============================================================================

cantApar :: Char -> Texto -> Int
cantApar c txt = length [x | x <- txt, x == c]

frecuency :: Char -> Texto -> Float
frecuency _ [] = 0.0
frecuency c txt = fromIntegral (cantApar c txt) / fromIntegral (length txt)

-- =============================================================================
-- # Ejercicio 2: definir una función frecuencyMap que dado un texto calcule la frecuencia con la que ocurre cada caracter del texto en éste.
-- La lista resultado debe estar ordenada respecto a la frecuencia con la que ocurre cada caracter, de menor a mayor frecuencia.
-- Por ejemplo: frecuencyMap "casa" = [('c',0.25),('s',0.25),('a',0.5)]
-- =============================================================================

mapFrec :: Texto -> [(Char, Float)]
mapFrec txt = [(c, frecuency c txt) | c <- nub txt]

{-
\* nub toma una lista y devuelve una lista con los elementos de la misma sin repetirlos. O sea, elimina las apariciones más allá de la primera aparición de un elemento
-}

frecuencyMap :: Texto -> [(Char, Float)]
frecuencyMap txt = sortBy (comparing snd) (mapFrec txt)

{-
\* sortBy ordena una lista según lo establecido por el usuario. Para decirle a la función qué debe comparar en cada elemento, usamos comparing.
\* En este caso, comparing snd indica que comparamos por el segundo elemento de cada tupla de la lista.
-}

-- =============================================================================
-- # Ejercicio 3: definir una función subconjuntos, que dada una lista xs devuelva una lista con las listas que pueden generarse con los elementos de xs.Por ejemplo: subconjuntos [2,3,4] = [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
-- =============================================================================

subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]]
subconjuntos (x : xs) = sinX ++ conX
  where
    sinX = subconjuntos xs
    conX = [x : ys | ys <- sinX]

-- =============================================================================
-- # Ejercicio 4: definir una función intercala :: a -> [a] -> [[a]] tal que (intercala x ys) contiene las listas que se obtienen intercalando x entre los elementos de ys.
-- Por ejemplo: intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
-- =============================================================================

intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y : ys) = (x : y : ys) : [y : zs | zs <- intercala x ys]

-- =============================================================================
-- # Ejercicio 5: definir una función permutaciones que dada una lista calcule todas las permutaciones posibles de sus elementos. Ayuda: la función anterior puede ser útil.
-- Por ejemplo: permutaciones "abc" = ["abc","bac","cba","bca","cab","acb"]
-- =============================================================================

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x : xs) = concat [intercala x zs | zs <- permsResto]
  where
    permsResto = permutaciones xs