{-
Devuleve el prefijo más grande que cumple un predicado.
-}
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

{-
Definir la función dropWhile que elimina el prefijo más grande que cumple un predicado.
-}
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = x : xs

{-
Definir la funcón span que divide una lista entre el prefijo más grande que cumple un predicado y el resto de la lista.
-}
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p [] = ([], []) -- se puede obviar porque ya las funciones anteriores lo contemplarían
span' p xs = (takeWhile' p xs, dropWhile' p xs)