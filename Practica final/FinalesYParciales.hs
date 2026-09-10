module FinalesYParciales where

import Seq
import Prelude hiding (drop, filter, length, map, take)

(|||) :: a -> b -> (a, b)
a ||| b = (a, b)

{-
================================================================================
Parcial: 14/06/2023

Se representan secuencias mediante árboles binarios dados por el siguiente tipo de datos:
data Tree a = E | N Int (Tree a) a (Tree a)
donde se guarda la longitud de la secuencia en los nodos y el recorrido inorder del árbol da el orden de los elementos de la secuencia.

Definir en Haskell de manera eficiente la función
filterPrefix :: (a -> Bool) -> Tree a -> Tree a, que dado un predicado p y una secuencia s, computa el prefijo más largo de s para el cual todos sus elementos satisfacen p.

Por ejemplo,
filterPrefix odd ⟨6, 6, 8, 1, 4, 5⟩ = ⟨⟩
filterPrefix even ⟨6, 6, 8, 1, 4, 5⟩ = ⟨6, 6, 8⟩

Definir filterPrefix con profundidad en O(h), donde h es la altura del árbol y p es de costo constante.
================================================================================
-}
data TreeA a = EA | NA Int (TreeA a) a (TreeA a)

filterPrefix :: (a -> Bool) -> TreeA a -> TreeA a
filterPrefix _ EA = EA
filterPrefix p (NA t izq x der)
  | sizeA izq' < sizeA izq = izq'
  | p x = NA (sizeA izq + 1 + sizeA der') izq x der'
  | otherwise = izq
  where
    (izq', der') = filterPrefix p izq ||| filterPrefix p der

sizeA :: TreeA a -> Int
sizeA EA = 0
sizeA (NA t _ _ _) = t

{-
================================================================================
Final: 02/02/2026

Suponiendo una representación de secuencias con árboles binarios, definidos con el siguiente tipo de datos:
data Tree a = E | N Int (Tree a) (Tree a) | L a
donde el recorrido inorder del árbol da el orden de los elementos de la secuencia y se almacena la información sobre el tamaño de los árboles en los nodos.

Definir de manera eficiente divide :: Tree a -> Int -> (Tree a, Tree a), que dada una secuencia s y un natural n, divida la secuencia en dos con los primeros n elementos de s y el resto de los elementos.

Por ejemplo,
divide ⟨x0, x1, x2, x3, x4⟩ 3 = (⟨x0, x1, x2⟩, ⟨x3, x4⟩)

================================================================================
-}
data TreeB a = EB | NB Int (TreeB a) (TreeB a) | LB a

divide :: TreeB a -> Int -> (TreeB a, TreeB a)
divide arbol 0 = (EB, arbol)
divide EB _ = (EB, EB)
divide (LB x) _ = (LB x, EB)
divide t n | n >= sizeB t = (t, EB)
divide (NB t izq der) n
  | n <= sizeB izq =
      let (a, b) = divide izq n
       in (a, joinB b der)
  | otherwise =
      let (a, b) = divide der (n - sizeB izq)
       in (joinB izq a, b)

joinB :: TreeB a -> TreeB a -> TreeB a
joinB t1 EB = t1
joinB EB t2 = t2
joinB t1 t2 = NB (sizeB t1 + sizeB t2) t1 t2

sizeB :: TreeB a -> Int
sizeB EB = 0
sizeB (LB _) = 1
sizeB (NB t _ _) = t

{-
================================================================================
Final: 02/02/2026

data Tree a = E | N Int (Tree a) (Tree a) | L a
Definir de manera eficiente interleave :: Tree a -> Tree a -> Tree a, que dadas dos secuencias s y s' devuelva una secuencia de largo |s| + |s'| con los elementos de s y s' de manera intercalada.

Por ejemplo
interleave ⟨x0,x1,x2,x3⟩ ⟨y0,y1,y2⟩ = ⟨x0,y0,x1,y1,x2,y2,x3⟩

Definir interleave de manera que en el caso en que los árboles tengan la misma estructura, la función tenga profundidad en O(h), siendo h la altura de los árboles. Dar las recurrencias correspondientes al trabajo y la profundidad de interleave para este caso. No hace falta resolverlas.
================================================================================
-}
interleave :: TreeB a -> TreeB a -> TreeB a
interleave t1 EB = t1
interleave EB t2 = t2
interleave (LB x) t2 = joinB (LB x) t2
interleave (NB t izq der) t2 =
  let (eqIzq, resto) = divide t2 (sizeB izq)
      (izq', der') = interleave izq eqIzq ||| interleave der resto
   in joinB izq' der'

{-
W(sizeB), W(joinB) \in O(1)
S(sizeB), S(joinB) \in O(1)

W(h) = 2*W(h-1) + O(1)
S(h) = S(h-1) + O(1)
-}

{-
================================================================================
Final: 02/02/2026

Dada la siguiente definición para representar árboles generales en Haskell:
data GTree a = Node a [GTree a]

definir las siguientes funciones:
a) elemT :: Eq a => a -> GTree a -> Bool, que determina si un elemento está en el árbol.
b) descendent :: Eq a => a -> a -> GTree a -> Bool, que dados dos valores x e y y un árbol t, determina si existe un nodo en t cuyo valor sea y y que sea descendiente de otro nodo en t cuyo valor sea x.

Por ejemplo:
descendent 3 5 (Node 3 [Node 4 [], Node 5 []]) = True
descendent 3 5 (Node 4 [Node 3 [], Node 5 []]) = False
================================================================================
-}
data GTree a = GNode a [GTree a] deriving (Show)

elemT :: (Eq a) => a -> GTree a -> Bool
elemT x (GNode y []) = x == y
elemT x (GNode y hijos) = x == y || or (map (elemT x) hijos)

descendent :: (Eq a) => a -> a -> GTree a -> Bool
descendent x y (GNode z hijos) =
  (x == z && or (map (elemT y) hijos)) || or (map (descendent x y) hijos)

{-
================================================================================
Parcial: 23/06/2025

Se representan secuencias mediante árboles binarios dados por el siguiente tipo de datos:
data Tree a = Leaf a | Node Int (Tree a) (Tree a)
donde se guarda la longitud de la secuencia en los nodos y el recorrido inorder del árbol da el orden de los elementos de la secuencia.

a) Definir en Haskell de manera eficiente la función
mapReduceIndex :: (Int -> a -> b) -> (b -> b -> b) -> Tree a -> b
que dados una función f :: Int -> a -> b, un operador binario asociativo op :: b -> b -> b y una secuencia s, computa el resultado de aplicar el operador op sobre los elementos de la secuencia que resulta de aplicar la función f a cada elemento de s usando como primer argumento su índice en s, en el orden dado por el recorrido inorder.

Por ejemplo,
mapReduceIndex f op ⟨x0, x1, x2⟩ = ((f 0 x0) `op` (f 1 x1)) `op` (f 2 x2)

Definir mapReduceIndex con profundidad en O(h), donde h es la altura del árbol y f y op son de costo constante.
Plantear las recurrencias para el trabajo y la profundidad para el caso en que f y op son de costo constante. No hace falta resolverlas.

b) Usando la función mapReduceIndex, definir una función sumEven :: Tree Int -> Int que compute la suma de los números en las posiciones pares de una secuencia.

Por ejemplo, sumEven ⟨7, 1, 5, 7, 2⟩ = 14
================================================================================
-}
data TreeD a = LD a | ND Int (TreeD a) (TreeD a)

mapReduceIndex :: (Int -> a -> b) -> (b -> b -> b) -> TreeD a -> b
mapReduceIndex f op arbol = mapReduceIndexAux f op arbol 0

mapReduceIndexAux :: (Int -> a -> b) -> (b -> b -> b) -> TreeD a -> Int -> b
mapReduceIndexAux f _ (LD x) i = f i x
mapReduceIndexAux f op (ND _ izq der) i =
  let (valIzq, valDer) = mapReduceIndexAux f op izq i ||| mapReduceIndexAux f op der (i + sizeD izq)
   in valIzq `op` valDer

sizeD :: TreeD a -> Int
sizeD (LD _) = 1
sizeD (ND t _ _) = t

sumEven :: TreeD Int -> Int
sumEven arbol = mapReduceIndex (\i x -> if even i then x else 0) (+) arbol

{-
================================================================================
Parcial: 12/06/2024

Se usan estos tipos:
data MultiDic k v = E | N (MultiDic k v) (k, Tree v) (MultiDic k v)
data Tree a = Empty | Leaf a | Node Int (Tree a) (Tree a)

El MultiDic es un árbol binario de búsqueda ordenado por clave, donde cada nodo guarda un par (clave, árbol de valores). El Tree guarda los valores en las hojas y la cantidad de nodos en el Int.

Definir de manera eficiente:
toMap :: Ord k => MultiDic k v -> Tree (k, Int, v)
similar a la función toMap del TAD, pero que agrega a cada par de la forma (clave, valor) la posición del valor en la secuencia representada por el árbol.

Por ejemplo,
toMap ⟨(1, ⟨a,f,g⟩), (2, ⟨m,a⟩)⟩ = ⟨(1,0,a), (1,1,f), (1,2,g), (2,0,m), (2,1,a)⟩
================================================================================
-}
data MDicE k v = EE | NE (MDicE k v) (k, TreeE v) (MDicE k v)

data TreeE a = EmptyE | LeafE a | NodeE Int (TreeE a) (TreeE a)

toMap :: (Ord k) => MDicE k v -> TreeE (k, Int, v)
toMap EE = EmptyE
toMap (NE izq (k, vals) der) =
  let (izq', der') = toMap izq ||| toMap der
      mid = keyIndTree k 0 vals
   in joinE izq' (joinE mid der')

keyIndTree :: k -> Int -> TreeE v -> TreeE (k, Int, v)
keyIndTree _ _ EmptyE = EmptyE
keyIndTree k i (LeafE x) = LeafE (k, i, x)
keyIndTree k i (NodeE t izq der) =
  let (izq', der') = keyIndTree k i izq ||| keyIndTree k (i + sizeE izq) der
   in NodeE t izq' der'

joinE :: TreeE a -> TreeE a -> TreeE a
joinE t1 EmptyE = t1
joinE EmptyE t2 = t2
joinE t1 t2 = NodeE (sizeE t1 + sizeE t2) t1 t2

sizeE :: TreeE a -> Int
sizeE EmptyE = 0
sizeE (LeafE _) = 1
sizeE (NodeE t _ _) = t

{-
================================================================================
Final: 02/02/2026

Dada una secuencia de valores numéricos, decimos que el valor de la posición i-ésima es mayor a su promedio histórico si es mayor al promedio de todos los anteriores. Por ejemplo, en la secuencia ⟨1, 5, 2⟩, los primeros dos elementos son mayores a su promedio histórico, mientras que el último no (para el primer elemento, consideramos que su promedio histórico es 0).

Usando las funciones del TAD secuencia, definir una función
highestIndexes : Seq Float → Seq Nat
que dada una secuencia s, devuelva una secuencia con los índices de s que corresponden a valores mayores a su promedio histórico.

Por ejemplo:
highestIndexes ⟨1, 5, 2⟩ = ⟨0, 1⟩
highestIndexes ⟨-2, -5⟩ = ⟨⟩

Definir highestIndexes con profundidad en O(lg n), donde n es la longitud de la secuencia.
================================================================================
-}

highestIndexes :: Seq Float -> Seq Int
highestIndexes s =
  let n = length s
      (sumas, _) = scan (+) 0.0 s
      ternas = tabulate (\i -> (nth s i, if i == 0 then 0 else nth sumas i / fromIntegral i, i)) n
      buenas = filter (\(val, prom, _) -> val > prom) ternas
   in map (\(_, _, i) -> i) buenas

{-
================================================================================
Final: 29/06/2023

Un evento de ola de frío se define cuando tanto las temperaturas máximas como las mínimas son iguales o inferiores, por lo menos durante 3 días consecutivos, a cierto valor que depende de cada localidad (percentil 10 del semestre frío abril-agosto).

Dada una secuencia de valores de pares numéricos que representan las temperaturas máximas y mínimas diarias, y un valor v que representa el percentil 10 de una localidad, se quiere calcular cuánto duró la ola de frío más larga.

Definir una función
coldWaveDays : Seq (Float, Float) → Float → Nat
que dada una secuencia de pares de temperaturas y un valor v, devuelva la cantidad de días de la ola de frío más larga.

coldWaveDays ⟨(10,5), (15,4), (10,3)⟩ 6 = 0
coldWaveDays ⟨(10,5), (5,4), (10,3)⟩ 6 = 0
coldWaveDays ⟨(8,5), (7,5), (6,5), (5,4), (10,3), (2,5), (5,2)⟩ 7 = 3

Notar que en el primer ejemplo no hubo ningún día con temperatura máxima y mínima menor o igual a 6; en el segundo hubo sólo un día con ambas temperaturas iguales o menores a 6, pero se necesitan al menos 3 días consecutivos para que sea una ola de frío; mientras que en el tercer ejemplo, las temperaturas (7,5), (6,5) y (5,4) son todas inferiores o iguales a 7 y además corresponden a 3 días consecutivos.

Definir coldWaveDays con profundidad en O(lg n), donde n es la longitud de la secuencia.
================================================================================
-}
combineA :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineA (s1, t1) (s2, t2) = (s, t)
  where
    s = if s2 == t2 then s1 + s2 else s2
    t = t1 + t2

baseA :: Float -> (Float, Float) -> (Int, Int)
baseA p (minT, maxT) = if maxVal <= p then (1, 1) else (0, 1)
  where
    maxVal = max minT maxT

coldWaveDays :: Seq (Float, Float) -> Float -> Int
coldWaveDays s v =
  let tuplas = map (baseA v) s
      (prefs, ult) = scan combineA (0, 0) tuplas
      temps = append (drop prefs 1) (singleton ult)
      sufs = map fst temps
      n = reduce max 0 sufs
   in if n < 3 then 0 else n

{-
================================================================================
Parcial: 14/06/2023

Usando las funciones del TAD secuencia, incluyendo necesariamente a la función scan, definir una función

longestStreak : Float → Seq Float → Int
que dados un valor val que representa una temperatura, y una secuencia s que representa la temperatura máxima diaria a lo largo del tiempo, calcule la racha más larga de días en s donde la temperatura superó los val grados.

longestStreak 30 ⟨20, 21, 27, 30, 24⟩ = 0
longestStreak 30 ⟨28, 31, 32, 29, 31, 31, 33, 29⟩ = 3
longestStreak 30 ⟨28, 31, 29, 31, 29⟩ = 1

Definir longestStreak con profundidad en O(lg n).
================================================================================
-}
combineB :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineB (s1, t1) (s2, t2) = (s, t)
  where
    s = if s2 == t2 then s1 + s2 else s2
    t = t1 + t2

baseB :: Float -> Float -> (Int, Int)
baseB val temp = if temp > val then (1, 1) else (0, 1)

longestStreak :: Float -> Seq Float -> Int
longestStreak val s =
  let baseS = map (baseB val) s
      (prefs, ult) = scan combineB (0, 0) baseS
      unida = append (drop prefs 1) (singleton ult)
      sufs = map fst unida
   in reduce max 0 sufs

{-
================================================================================
Parcial: 16/06/2025

Para realizar operaciones de compra y venta de dólares se definieron los siguientes tipos de datos:
type Pesos = Float
type Dolares = Float
type Cotizacion = Float
data Transaccion = Depositar Pesos | Comprar Int Cotizacion | Vender Int Cotizacion

donde Depositar x representa "depositar x pesos", Comprar n c "comprar n dólares con costo c pesos cada uno" y Vender n c "vender n dólares con la cotización c".

La siguiente función calcula el saldo en pesos y dólares luego de una transacción:
eval :: Transaccion -> (Pesos, Dolares)
eval (Depositar pesos) = (pesos, 0)
eval (Comprar n cot) = let x = fromIntegral n in (-x * cot, x)
eval (Vender n cot) = let x = fromIntegral n in (x * cot, -x)

Una secuencia de transacciones es correcta si al evaluar cada transacción en el orden en que ocurren todas pueden realizarse. Por ejemplo, transOk es correcta, mientras que transFail1 y transFail2 no (no pueden venderse dólares sin haberlos comprado antes, ni comprarse sin saldo suficiente en pesos).

transOk    = [Depositar 100000, Comprar 100 1000, Vender 20 500, Comprar 5 1000]
transFail1 = [Depositar 100000, Vender 20 500, Comprar 100 1000, Comprar 5 1000]
transFail2 = [Depositar 100, Comprar 100 1000]

Usando las operaciones del TAD Secuencias, definir una función transaccionOk, que dada una secuencia de transacciones, determine si es correcta. Definir esta función con profundidad en O(lg n) y trabajo en O(n).
================================================================================
-}
type Pesos = Float

type Dolares = Float

type Cotizacion = Float

data Transaccion = Depositar Pesos | Comprar Int Cotizacion | Vender Int Cotizacion

eval :: Transaccion -> (Pesos, Dolares)
eval (Depositar pesos) = (pesos, 0)
eval (Comprar n cot) = let x = fromIntegral n in (-x * cot, x)
eval (Vender n cot) = let x = fromIntegral n in (x * cot, -x)

transaccionOk :: Seq Transaccion -> Bool
transaccionOk s =
  let mapEval = map eval s
      (acumVals, ult) = scan sumTrans (0, 0) mapEval
      unida = append (drop acumVals 1) (singleton ult)
      finalS = filter (\(p, d) -> p >= 0 && d >= 0) unida
   in length finalS == length unida

sumTrans :: (Pesos, Dolares) -> (Pesos, Dolares) -> (Pesos, Dolares)
sumTrans (p1, d1) (p2, d2) = (p1 + p2, d1 + d2)

{-
================================================================================
Parcial 18/06/2026

Dados los diferentes valores de las acciones de YPF a lo largo del tiempo (representados como una secuencia de valores que representan el valor de las acciones cada día), se desea saber cuál es la mejor ganancia que se puede obtener al comprar acciones un día y venderlas otro día posterior.

mejorGanancia ⟨10, 7, 5, 8, 11, 9⟩ = 6
mejorGanancia ⟨10, 9, 8, 7⟩ = 0
mejorGanancia ⟨12⟩ = 0

En la primera secuencia la mejor ganancia 6 se obtiene comprando a 5 y vendiendo a 11; en la segunda es 0 porque no hay ganancia posible; en la tercera es 0 porque no hay suficientes días para comprar y vender.

Utilizando las operaciones del TAD secuencias, completar la definición de la función mejorGanancia : Seq Int → Int, dando definiciones para reverse, op y b de manera que se verifique profundidad en O(lg n) y trabajo en O(n).

Definir reverse, op y b.
================================================================================
-}
mejorGanancia :: Seq Int -> Int
mejorGanancia s =
  let n = length s
      mejorVenta = reverseS (fst (scan op b (reverseS s)))
      diff = tabulate (\i -> nth mejorVenta i - nth s i) (n - 1)
   in if n < 2 then 0 else reduce max 0 diff

reverseS :: Seq a -> Seq a
reverseS s =
  let n = length s
   in tabulate (\i -> nth s (n - i - 1)) n

b = minBound

op = max

{-
================================================================================
Parcial: 12/06/2024

Decimos que una secuencia es una progresión aritmética si la diferencia entre cualquier elemento (a excepción del primero) y su anterior es un valor constante. Por ejemplo, ⟨3,5,7,9⟩ es una progresión aritmética mientras que ⟨3,5,6,8,9⟩ no lo es.

Se quiere definir una función spaml : Seq Int → Int que dada una secuencia de enteros devuelve la longitud de la subsecuencia contigua más larga que es progresión aritmética.

spaml ⟨7,6,5,4,6,8⟩ = 4
spaml ⟨6,7,5,6⟩ = 2
spaml ⟨1,2,5,8,12,14,16⟩ = 3

Completar la siguiente definición de spaml, dando definiciones apropiadas para f, g y h, de manera que spaml tenga profundidad en O(lg n).
-}

spaml :: Seq Int -> Int
spaml s
  | n <= 2 = n
  | otherwise = spaml_aux s
  where
    n = length s

spaml_aux :: Seq Int -> Int
spaml_aux s =
  let n = length s
      s_dif = tabulate (\i -> nth s (i + 1) - nth s i) (n - 1)
      s_info = map f s_dif
      (s_red, r) = scan g (nth s_info 0) (drop s_info 1)
      s_res = map h (append s_red (singleton r))
      f = baseC
      g = combineC
      h = suf
   in 1 + reduce max 0 s_res

combineC :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
combineC (s1, t1, p1, u1) (s2, t2, p2, u2) = (s, t, p, u)
  where
    s = if s2 == t2 && p2 == u1 then s1 + s2 else s2
    t = t1 + t2
    p = p1
    u = u2

baseC :: Int -> (Int, Int, Int, Int)
baseC d = (1, 1, d, d)

suf :: (Int, Int, Int, Int) -> Int
suf (s, _, _, _) = s