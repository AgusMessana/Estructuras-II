module Practica3 where

import Distribution.Compiler (PerCompilerFlavor (PerCompilerFlavor))

-- =============================================================================
-- # Ejercicio 1: definir un tipo Color en el modelo RGB y una función mezclar que permita obtener el promedio componente a componente entre dos colores.
-- =============================================================================

data Color = Color Float Float Float deriving (Show)

mezclar :: Color -> Color -> Color
mezclar (Color r1 g1 b1) (Color r2 g2 b2) =
  Color ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2)

-- =============================================================================
-- # Ejercicio 2: supongamos que una Línea es una secuencia de caracteres c1, c2, ..., cn junto con una posicion p, siendo 0 <= p <= n, llamada cursor (consideraremos al cursor a la derecha de un carácter que será borrado o insertado, es decir, como el cursor de la mayora de los editores). Se requieren las siguientes operaciones sobre líneas:
-- =============================================================================

data Linea = Linea [Char] Int [Char] deriving (Show)

vacia :: Linea
vacia = Linea [] 0 []

moverIzq :: Linea -> Linea
moverIzq (Linea [] pos der) = Linea [] pos der
moverIzq (Linea izq pos der) = Linea (init izq) (pos - 1) (last izq : der)

moverDer :: Linea -> Linea
moverDer (Linea izq pos []) = Linea izq pos []
moverDer (Linea izq pos der) = Linea (izq ++ [head der]) (pos + 1) (tail der)

moverIni :: Linea -> Linea
moverIni (Linea [] pos der) = Linea [] pos der
moverIni (Linea izq pos der) = Linea [] 0 (izq ++ der)

moverFin :: Linea -> Linea
moverFin (Linea izq pos []) = Linea izq pos []
moverFin (Linea izq pos der) = Linea (izq ++ der) (length izq + length der) []

insertar :: Char -> Linea -> Linea
insertar x (Linea [] pos der) = Linea [x] (pos + 1) der
insertar x (Linea izq pos der) = Linea (izq ++ [x]) (pos + 1) der

borrar :: Linea -> Linea
borrar (Linea [] pos der) = Linea [] pos der
borrar (Linea izq pos der) = Linea (init izq) (pos - 1) der

-- =============================================================================
-- # Ejercicio 3: dado el tipo de datos:
-- =============================================================================

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)

-- ### Inciso a): implementar las operaciones de este tipo algebráico

headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x l y) = x

tailCL :: CList a -> CList a
tailCL (CUnit x) = EmptyCL -- Porque la cola de una lista de un solo elemento es la lista vacía
tailCL (Consnoc x l y) = appendElem l y

-- Definimos appendElem que le agrega un elemento al final a una lista.

appendElem :: CList a -> a -> CList a
appendElem EmptyCL x = CUnit x -- A la lista vacía le agrego un solo elemento. Queda una lista de un solo elemento.
appendElem (CUnit y) x = Consnoc y EmptyCL x -- La del medio es vacía porque tengo dos elementos.
appendElem (Consnoc a l b) x = Consnoc a (appendElem l b) x

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL (CUnit x) = False
isEmptyCL (Consnoc x l y) = False

{-
Otra forma sería
isEmptyCL EmptyCL = True
isEmptyCL _ = False
-}

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _ = False

-- ### Inciso b): definir una función reverseCL que toma una CList y devuelve su inversa.

reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x l y) = Consnoc y (reverseCL l) x

-- ### Inciso c): definir una función inits que toma una CList y devuelve una CList con todos los posibles inicios de la CList.
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
inits c@(Consnoc x l y) = Consnoc EmptyCL m c -- El @ da un nombre a algo
  where
    m = mapPrepend (inits l) x

mapPrepend :: CList (CList a) -> a -> CList (CList a)
mapPrepend EmptyCL x = EmptyCL
mapPrepend (CUnit l) x = CUnit (consCL x l)
mapPrepend (Consnoc l1 ls l2) x = Consnoc (consCL x l1) (mapPrepend ls x) (consCL x l2)

consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit y) = Consnoc x EmptyCL y
consCL x (Consnoc a l b) = Consnoc x (consCL a l) b

-- ### Inciso d): definir una función lasts que toma una CList y devuelve una CList con todas las posibles terminaciones de la CList.
{-
Ejemplos
[] -> [[]]
[1] -> [[], [1]]
[1, 2] -> [[], [2], [1, 2]]
[1, 2, 3] -> [[], [3], [2, 3], [1, 2, 3]]
[1, 2, 3, 4] -> [[], [4], [3, 4], [2, 3, 4], [1, 2, 3, 4]]
-}

lasts :: CList a -> CList (CList a)
lasts EmptyCL = CUnit EmptyCL
lasts (CUnit x) = Consnoc EmptyCL EmptyCL (CUnit x)
lasts d@(Consnoc x l y) = Consnoc EmptyCL n d
  where
    n = mapPostpend (lasts l) y

mapPostpend :: CList (CList a) -> a -> CList (CList a)
mapPostpend EmptyCL y = EmptyCL
mapPostpend (CUnit l) y = CUnit (consFCL l y)
mapPostpend (Consnoc l1 l l2) y = Consnoc (consFCL l1 y) (mapPostpend l y) (consFCL l2 y)

consFCL :: CList a -> a -> CList a
consFCL EmptyCL y = CUnit y
consFCL (CUnit x) y = Consnoc x EmptyCL y
consFCL (Consnoc x l z) y = Consnoc x (consFCL l z) y

-- ### Inciso e): definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas concatenadas.

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit l) = l
concatCL (Consnoc l1 l l2) = consLCL l1 (consLCL (concatCL l) l2)

consLCL :: CList a -> CList a -> CList a
consLCL EmptyCL l2 = l2
consLCL (CUnit x) l2 = consCL x l2
consLCL (Consnoc x l1 y) l2 = consLCL (consCL x l1) (consCL y l2)

-- =============================================================================
-- # Ejercicio 4: definir un evaluador eval :: Exp -> Int para el siguiente tipo algebráico:
-- =============================================================================

data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp deriving (Show)

eval :: Exp -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = div (eval e1) (eval e2)

-- =============================================================================
-- # Ejercicio 5
-- =============================================================================

-- ### Inciso a): definir una función parseRPN :: String -> Exp que, dado un string que representa una expresion escrita en RPN, construya un elemento del tipo Exp presentado en el ejercicio 4 correspondiente a la expresión dada.

procesarToken :: [Exp] -> String -> [Exp]
procesarToken (e2 : e1 : pila) "+" = Add e1 e2 : pila
procesarToken (e2 : e1 : pila) "-" = Sub e1 e2 : pila
procesarToken (e2 : e1 : pila) "*" = Prod e1 e2 : pila
procesarToken (e2 : e1 : pila) "/" = Div e1 e2 : pila
procesarToken pila nroSting = Lit (read nroSting) : pila

parseRPN :: String -> Exp
parseRPN txt =
  let tokens = words txt
      pilaFinal = foldl procesarToken [] tokens
   in head pilaFinal

-- ### Inciso b): definir una función evalRPN :: String -> Int para evaluar expresiones aritméticas escritas en RPN.

evalRPN :: String -> Int
evalRPN txt =
  let formula = parseRPN txt
   in eval formula

-- =============================================================================
-- # Ejercicio 6
-- =============================================================================

-- ### Inciso a): considerar el evaluador eval :: Exp Int del ejercicio 4. ¿Cómo maneja los errores de division por 0?
-- Al momento de dividir por 0, se lanza una excepción de que no se puede dividir por 0.

-- ### Inciso b): definir un evaluador seval :: Exp -> Maybe Int para controlar los errores de división por 0.

seval :: Exp -> Maybe Int
seval (Lit n) = Just n
seval (Add e1 e2) =
  case (seval e1, seval e2) of
    (Just v1, Just v2) -> Just (v1 + v2)
    _ -> Nothing
seval (Sub e1 e2) =
  case (seval e1, seval e2) of
    (Just v1, Just v2) -> Just (v1 - v2)
    _ -> Nothing
seval (Prod e1 e2) =
  case (seval e1, seval e2) of
    (Just v1, Just v2) -> Just (v1 * v2)
    _ -> Nothing
seval (Div e1 e2) =
  case (seval e1, seval e2) of
    (Just v1, Just 0) -> Nothing
    (Just v1, Just v2) -> Just (div v1 v2)
    _ -> Nothing