-- =============================================================================
-- Parcial viejo: 23 / 06 / 2025
-- =============================================================================

(|||) :: a -> b -> (a, b)
a ||| b = (a, b)
-- =============================================================================
-- # Ejercicio 2
-- =============================================================================
data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving Show
-- =============================================================================
-- ### Inciso a
-- =============================================================================
mapReduceIndex :: (Int -> a -> b) -> (b -> b -> b) -> Tree a -> b
mapReduceIndex f op s = auxMRI f op s 0

auxMRI :: (Int -> a -> b) -> (b -> b -> b) -> Tree a -> Int -> b
auxMRI f _ (Leaf x) i = f i x
auxMRI f op (Node _ izq der) i =
  let (valI, valD) = auxMRI f op izq i ||| auxMRI f op der (size izq + i)
   in valI `op` valD

size :: Tree a -> Int
size (Leaf _) = 1
size (Node t _ _) = t

{-
W(size) = S(size) = c1
W(f) = S(f) = c2
W(op) = S(op) = c3

W(h) = 2*W(h - 1) + c1 + c2 + c3 \in O(2^h)
S(h) = W(h - 1) + c1 + c2 + c3 \in O(h)
-}
-- =============================================================================
-- ### Inciso b
-- =============================================================================
sumEven :: Tree Int -> Int
sumEven = mapReduceIndex (\i x -> if even i then x else 0) (+)

-- =============================================================================
-- # Ejercicio 3
-- =============================================================================
type Pesos = Float
type Dolares = Float
type Cotizacion = Float
data Transaccion = Depositar Pesos | Comprar Int Cotizacion | Vender Int Cotizacion

eval :: Transaccion -> (Pesos, Dolares)
eval (Depositar pesos) = (pesos, 0)
eval (Comprar n cot) = let x = fromIntegral n in (-x * cot, x)
eval (Vender n cot) = let x = fromIntegral n in (x * cot, -x)

transaccionOk : Seq Transaccion -> Bool
transaccionOk s =
  let sEval = map eval s
      (pref, ult) = scan sumPares (0, 0) sEval
      sPrefs = append (drop 1 pref) (singleton ult)
      sBool = map (\(peso, dolar) -> peso >= 0 && dolar >= 0) sPrefs
      val = reduce (&&) True sBool
   in val

sumPares :: (Pesos, Dolares) -> (Pesos, Dolares) -> (Pesos, Dolares)
sumPares (x, y) (x', y') = (x + x', y + y')
--------------------------------------------------------------------------------
-- =============================================================================
-- Parcial viejo: 14 / 06 / 2023
-- =============================================================================

-- =============================================================================
-- # Ejercicio 2
-- =============================================================================
data Tree' a = E | N Int (Tree' a) a (Tree' a) deriving Show
filterPrefix :: (a -> Bool) -> Tree' a -> Tree' a
filterPrefix _ E = E
filterPrefix p (N t izq x der)
  | size' izq' < size' izq = izq'
  | p x = N (size' izq + size' der' + 1) izq x der'
  | otherwise = izq
  where (izq', der') = filterPrefix p izq ||| filterPrefix p der

size' :: Tree' a -> Int
size' E = 0
size' (N t _ _ _) = t

-- =============================================================================
-- # Ejercicio 3
-- =============================================================================
base :: Float -> Float -> (Int, Int)
base val temp = if temp > val then (1, 1) else (0, 1)

op :: (Int, Int) -> (Int, Int) -> (Int, Int)
op (s1, t1) (s2, t2) = (if s2 == t2 then s2 + s1 else s2, t1 + t2)

longestStreak : Float -> Seq Float -> Int
longestStreak val s =
  let sRacha = map (base val) s
      (pref, tot) = scan op (0, 0) sRacha
      sUnida = append (drop 1 pref) (singleton tot)
      sTemps = map fst sUnida
   in reduce max 0 sTemps
