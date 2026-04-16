-- Ejercicio 3
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
--EmptyCL :: CList a
--CUnit :: a -> CList a
--Consnoc :: a -> CList a -> a -> CList a

-- a) iv)
tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL -- Porque la cola de una lista de un solo elemento es la lista vacía
tailCL (Consnoc x l y) = appendElem l y

-- Definimos appendElem que le agrega un elemnto al final a una lista
appendElem :: CList a -> a -> CList a
appendElem EmptyCL x = CUnit x -- A la lista vacía le agrego un solo elemento. Queda una lista de un solo elemento
appendElem (CUnit y) x = Consnoc y EmptyCL x -- La del medio es vacía porque tengo dos elementos
appendElem (Consnoc a l b) x = Consnoc a (appendElem l b) x

-- c)
{-
Ejemplos
[] -> [[]]
[1] -> [[], [1]]
[1, 2] -> [[], [1], [1, 2]]
[1, 2, 3] -> [[], [1], [1, 2], [1, 2, 3]]
[1, 2, 3, 4] -> [[], [1], [1, 2], [1, 2, 3], [1, 2, 3, 4]]
-}
inits :: CList a -> CList (CList a)
inits EmptyCL = CUnit EmptyCL
inits (CUnit x) = Consnoc EmptyCL EmptyCL (CUnit x)
inits c@(Consnoc x l y) = Consnoc EmptyCL m c -- El @ es para darle un nombre a algo
    where m = mapPrepend (inits l) x

mapPrepend :: CList (CList a) -> a -> CList (CList a)
mapPrepend EmptyCL x = EmptyCL
mapPrepend (CUnit l) x = CUnit (consCL x l)
mapPrepend (Consnoc l1 ls l2) x = Consnoc (consCL x l1) (mapPrepend ls x) (consCL x l2)

consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit y) = Consnoc x EmptyCL y
consCL x (Consnoc a l b) = Consnoc x (consCL a l) b