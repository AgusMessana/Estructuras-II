module Ej_1_4 where

import Data.List

{-
4) Defina un operador infijo *$ que implemente la multiplicación de un vector por un escalar. Representaremos a los vectores mediante listas de Haskell. Así, dada una lista ns y un número n, el valor ns *$ n debe ser igual a la lista ns con todos sus elementos multiplicados por n. Por ejemplo,

[ 2, 3 ] *$ 5 == [ 10 , 15 ].

El operador *$ debe definirse de manera que la siguiente
expresión sea válida:

v = [1, 2, 3] *$ 2 *$ 4
-}

infixl 7 *$ -- Para que tenga la misma asociatividad y precedencia que la multiplicación

(*$) :: (Num a) => [a] -> a -> [a]
ns *$ n = map (* n) ns