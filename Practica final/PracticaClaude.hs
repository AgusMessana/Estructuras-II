module PracticaClaude where

import Seq
import Prelude hiding (drop, filter, length, map, take)

{-
# Ejercicio 1
Una cadena de farmacias registra las ventas diarias de un producto a lo largo del tiempo. Decimos que hay un período de crecimiento sostenido cuando las ventas aumentan estrictamente día a día durante un tramo de días consecutivos.

Definir una función crecimientoMasLargo : Seq Int → Int que dada una secuencia de ventas diarias, devuelva la longitud del período de crecimiento sostenido más largo.

Por ejemplo:
crecimientoMasLargo ⟨3, 5, 8, 2, 4, 9, 11, 7⟩ = 4
crecimientoMasLargo ⟨9, 7, 5, 3⟩ = 1
crecimientoMasLargo ⟨1, 2, 3, 4, 5⟩ = 5
crecimientoMasLargo ⟨⟩ = 0

En el primer ejemplo, el tramo ⟨2,4,9,11⟩ crece estrictamente y tiene 4 días. En el segundo, no hay dos días consecutivos que crezcan, así que el mejor tramo es de un solo día.
Definir crecimientoMasLargo con profundidad en O(lg n) y trabajo en O(n).
-}

combine1 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
combine1 (s1, t1, p1, u1) (s2, t2, p2, u2) = (s, t, p, u)
  where
    s = if s2 == t2 && p2 > u1 then s2 + s1 else s2
    t = t1 + t2
    p = p1
    u = u2

base1 :: Int -> (Int, Int, Int, Int)
base1 v = (1, 1, v, v)

crecimientoMasLargo :: Seq Int -> Int
crecimientoMasLargo s
  | length s == 0 = 0
  | otherwise =
      let mappedS = map base1 s
          (prefs, ult) = scan combine1 (nth mappedS 0) (drop mappedS 1)
          unida = append prefs (singleton ult)
          sufijos = map (\(su, _, _, _) -> su) unida
       in reduce max 0 sufijos

{-
# Ejercicio 2
Una app de running registra, para cada kilómetro de una carrera, el tiempo en segundos que tardó el corredor en completarlo. Se quiere identificar el mejor tramo de ritmo constante: el tramo más largo de kilómetros consecutivos donde el tiempo por kilómetro se mantuvo dentro de un margen de tolerancia respecto del primer kilómetro del tramo.
Para simplificar, decimos que un tramo tiene ritmo constante si todos sus kilómetros tardaron exactamente lo mismo.

Definir una función mejorRitmo : Seq Int → Int que dada la secuencia de tiempos por kilómetro, devuelva la longitud del tramo de ritmo constante más largo.

Por ejemplo:
mejorRitmo ⟨300, 305, 305, 305, 310, 308⟩ = 3
mejorRitmo ⟨290, 291, 292⟩ = 1
mejorRitmo ⟨280, 280, 280, 280⟩ = 4
mejorRitmo ⟨⟩ = 0
En el primero, los tres kilómetros de 305 segundos forman el tramo más largo.

Definir mejorRitmo con profundidad en O(lg n) y trabajo en O(n).
-}
base2 :: Int -> (Int, Int, Int, Int)
base2 t = (1, 1, t, t)

combine2 :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
combine2 (s1, t1, p1, u1) (s2, t2, p2, u2) = (s, t, p, u)
  where
    s = if s2 == t2 && u1 == p2 then s1 + s2 else s2
    t = t1 + t2
    p = p1
    u = u2

mejorRitmo :: Seq Int -> Int
mejorRitmo s
  | length s == 0 = 0
  | otherwise =
      let mappedS = map base2 s
          (prefs, ult) = scan combine2 (nth mappedS 0) (drop mappedS 1)
          unitedS = append prefs (singleton ult)
          sufsS = map (\(suf, _, _, _) -> suf) unitedS
       in reduce max 0 sufsS

{-
# Ejercicio 3
Una biblioteca registra, para cada día del mes, la cantidad de libros prestados. Se quiere saber en qué días la cantidad de préstamos fue mayor a la de todos los días anteriores (un "récord histórico").

recordsHistoricos : Seq Int → Seq Nat devuelve los índices de los días que fueron récord. El primer día siempre cuenta como récord.

recordsHistoricos ⟨5, 3, 8, 8, 12, 1⟩ = ⟨0, 2, 4⟩
recordsHistoricos ⟨9, 7, 5⟩ = ⟨0⟩
Profundidad O(lg n).
-}
recordsHistoricos :: Seq Int -> Seq Int
recordsHistoricos s =
  let n = length s
      (maxAnt, _) = scan max minBound s
      ternas = tabulate (\i -> (nth s i, nth maxAnt i, i)) n
      filteredS = filter (\(v, m, _) -> v > m) ternas
   in map (\(_, _, ind) -> ind) filteredS

{-
# Ejercicio 4
Un curso tiene varios estudiantes, y de cada uno se conoce su nombre y la secuencia de notas que sacó en los parciales. Se quiere obtener, para cada estudiante, su nombre junto con su promedio, pero solo de los estudiantes que aprobaron todos los parciales (nota mayor o igual a 6 en todos).

aprobados : Seq (String, Seq Int) → Seq (String, Float)
aprobados ⟨("Ana", ⟨8,7,9⟩), ("Beto", ⟨6,4,8⟩), ("Cami", ⟨6,6,6⟩)⟩
  = ⟨("Ana", 8.0), ("Cami", 6.0)⟩

Profundidad O(lg n), donde n es la cantidad de estudiantes. Podés asumir que todos tienen la misma cantidad de parciales.
-}
aprobados :: Seq (String, Seq Int) -> Seq (String, Float)
aprobados s =
  let pasaron = filter (\(_, notas) -> notasMayores notas) s
   in map (\(st, aprob) -> (st, promedio aprob)) pasaron

promedio :: Seq Int -> Float
promedio s =
  let n = length s
      prom = reduce (+) 0 s
   in fromIntegral prom / fromIntegral n

notasMayores :: Seq Int -> Bool
notasMayores s =
  let boolNotas = map (>= 6) s
   in reduce (&&) True boolNotas

{-
# Ejercicio 5
Una empresa de logística registra el peso de cada paquete que sale de un depósito, en el orden en que salieron. Un camión tiene capacidad máxima cap. Se quiere saber cuántos paquetes entran en el camión si se cargan en orden desde el primero, sin superar la capacidad.

paquetesQueEntran : Int → Seq Int → Int
paquetesQueEntran 100 ⟨30, 40, 50, 10⟩ = 2
paquetesQueEntran 100 ⟨120, 5⟩ = 0
paquetesQueEntran 100 ⟨10, 20, 30⟩ = 3

Profundidad O(lg n).
-}
paquetesQueEntran :: Int -> Seq Int -> Int
paquetesQueEntran cap s =
  let (acumVals, ult) = scan (+) 0 s
      unitedVals = append (drop acumVals 1) (singleton ult)
      posVals = filter (\x -> x <= cap) unitedVals
   in length posVals

{-
# Ejercicio 6
Un local de comidas registra, para cada mesa atendida en el día, el monto que gastó y la cantidad de comensales. Se quiere calcular el gasto promedio por persona considerando únicamente las mesas cuyo gasto por persona superó el promedio general del día.

mesasSobrePromedio : Seq (Float, Int) → Float, donde cada par es (monto total de la mesa, cantidad de comensales). El "gasto por persona" de una mesa es monto / comensales. El promedio general del día es el promedio de los gastos por persona de todas las mesas.
Si ninguna mesa supera el promedio, devolver 0.

mesasSobrePromedio ⟨(100, 2), (90, 3), (200, 4)⟩ = 50.0
mesasSobrePromedio ⟨(60, 2), (60, 2)⟩ = 0.0
En el primero, los gastos por persona son 50, 30 y 50. El promedio general es 43.33. Las mesas que lo superan son la primera y la tercera, ambas con 50, así que el promedio de esas es 50.

Profundidad O(lg n), trabajo O(n).
-}
mesasSobrePromedio :: Seq (Float, Int) -> Float
mesasSobrePromedio s
  | length s == 0 = 0.0
  | otherwise =
      let proms = map (\(val, cant) -> val / fromIntegral cant) s
          sumaProms = reduce (+) 0 proms
          generalProm = sumaProms / fromIntegral (length s)
          mProms = filter (\f -> f > generalProm) proms
          fProms = reduce (+) 0 mProms
       in if length mProms == 0 then 0 else fProms / fromIntegral (length mProms)

{-
# Ejercicio 7
Una empresa de energía registra el consumo eléctrico de un edificio cada hora, durante varios días. Se quiere detectar los picos de consumo: horas en las que el consumo fue mayor tanto al de la hora anterior como al de la hora siguiente.

picosDeConsumo : Seq Int → Seq Int, devuelve los índices de las horas que fueron pico. La primera y la última hora nunca pueden ser pico (les falta un vecino).

picosDeConsumo ⟨3, 7, 4, 9, 2, 5⟩ = ⟨1, 3⟩
picosDeConsumo ⟨1, 2, 3, 4⟩ = ⟨⟩
picosDeConsumo ⟨5, 1, 5⟩ = ⟨⟩

En el primero, la hora 1 (consumo 7) supera a la 0 (3) y a la 2 (4). La hora 3 (consumo 9) supera a la 2 (4) y a la 4 (2).

Profundidad O(lg n).
-}
picosDeConsumo :: Seq Int -> Seq Int
picosDeConsumo s
  | length s <= 2 = emptyS
  | otherwise =
    let n = length s
        verVecinos = tabulate (\i -> 
          (nth s (i+1) > nth s i && nth s (i+1) > nth s (i+2), i+1)) (n - 2)
        filtrados = filter fst verVecinos
     in map snd filtrados

{-
# Ejercicio 8
Una fábrica registra la producción diaria de una máquina. Se quiere saber cuántos días llevó la máquina funcionando sin fallas desde el inicio del registro: o sea, la cantidad de días iniciales consecutivos en los que la producción fue mayor a 0, hasta el primer día con producción 0.

diasSinFallaInicial : Seq Int → Int
diasSinFallaInicial ⟨5, 8, 3, 0, 7, 2⟩ = 3
diasSinFallaInicial ⟨0, 4, 5⟩ = 0
diasSinFallaInicial ⟨6, 6, 6⟩ = 3
diasSinFallaInicial ⟨⟩ = 0

Profundidad O(lg n).
-}
diasSinFallaInicial :: Seq Int -> Int
diasSinFallaInicial s =
  let n = length s
      indexS = tabulate (\i -> (nth s i, i)) n
      filtrada = filter (\(v, _) -> v == 0) indexS
   in if length filtrada == 0 then n else snd (nth filtrada 0)

{-
# Ejercicio 9
Un servidor registra el tiempo de respuesta (en ms) de cada consulta que atendió. Se quiere saber cuántas consultas seguidas al final del registro tuvieron un tiempo de respuesta menor a 200 ms — o sea, cuántas consultas lleva el servidor respondiendo rápido hasta el momento actual.

consultasRapidasFinales : Seq Int → Int
consultasRapidasFinales ⟨150, 300, 180, 120, 90⟩ = 3
consultasRapidasFinales ⟨100, 120, 400⟩ = 0
consultasRapidasFinales ⟨50, 60, 70⟩ = 3
consultasRapidasFinales ⟨⟩ = 0

Profundidad O(lg n).
-}
consultasRapidasFinales :: Seq Int -> Int
consultasRapidasFinales s =
  let n = length s
      reversedS = reverseS s
      indexS = tabulate (\i -> (nth reversedS i, i)) n
      filtrada = filter (\(v, _) -> v >= 200) indexS
   in if length filtrada == 0 then n else snd (nth filtrada 0)

reverseS :: Seq a -> Seq a
reverseS s =
  let n = length s
   in tabulate (\i -> nth s (n - i - 1)) n

{-
# Ejercicio 10
Un embalse registra el nivel de agua cada semana. Para cada semana, se quiere saber cuántas semanas faltan hasta que el nivel vuelva a ser al menos tan alto como el actual. Si nunca vuelve a alcanzarlo, se devuelve 0.

semanasHastaRecuperar : Seq Int → Seq Int
semanasHastaRecuperar ⟨5, 3, 4, 6, 2⟩ = ⟨3, 1, 1, 0, 0⟩
(Desde la semana 0 con nivel 5, hay que esperar 3 semanas hasta el 6. Desde la 1 con nivel 3, una semana hasta el 4.)

Profundidad O(lg n)... y pensá si eso es alcanzable. Si concluís que no, decime por qué y resolvelo con la mejor profundidad que puedas justificar.
-}
semanasHastaRecuperar :: Seq Int -> Seq Int
semanasHastaRecuperar s =
  let n = length s
  in tabulate (\i -> primeraQueAlcanza (nth s i) (drop s (i+1))) n

primeraQueAlcanza :: Int -> Seq Int -> Int
primeraQueAlcanza val s = 
  let n = length s
      indexS = tabulate (\i -> (nth s i, i)) n
      filtrada = filter (\(v, _) -> v >= val) indexS
   in if length filtrada == 0 then 0 else snd (nth filtrada 0) + 1


{-
# Ejercicio 11
Una app de finanzas registra los movimientos de una cuenta (positivos son ingresos, negativos son gastos). Se quiere saber si la cuenta estuvo en descubierto en algún momento, y de ser así, cuál fue el saldo más negativo al que llegó. Si nunca estuvo en descubierto, devolver 0.

peorDescubierto : Seq Float → Float
peorDescubierto ⟨100, -30, -90, 50⟩ = -20.0
peorDescubierto ⟨100, -30, 20⟩ = 0.0

Profundidad O(lg n).
-}
peorDescubierto :: Seq Float -> Float
peorDescubierto s =
  let (prefs, ult) = scan (+) 0.0 s
      vals = append (drop prefs 1) (singleton ult)
      rta = reduce min (1/0) vals
   in if rta >= 0 then 0 else rta

{-
# Ejercicio 12
Un torneo de ajedrez registra los resultados de un jugador partida por partida: 1 si ganó, 0 si empató, -1 si perdió. Se quiere saber la mayor cantidad de partidas consecutivas sin perder (o sea, ganando o empatando).

mayorInvicto : Seq Int → Int
mayorInvicto ⟨1, 0, 1, -1, 0, 0, 1, 1, -1⟩ = 4
mayorInvicto ⟨-1, -1⟩ = 0
mayorInvicto ⟨1, 1, 1⟩ = 3

Profundidad O(lg n).
-}
combine3 :: (Int, Int) -> (Int, Int) -> (Int, Int)
combine3 (s1, t1) (s2, t2) = (s, t)
  where
    s = if s2 /= t2 then s2 else s1 + s2
    t = t1 + t2

base3 :: Int -> (Int, Int)
base3 r = if r < 0 then (0, 1) else (1, 1)

mayorInvicto :: Seq Int -> Int
mayorInvicto s =
  let tuplas = map base3 s
      (prefs, ult) = scan combine3 (0, 0) tuplas
      unida = append (drop prefs 1) (singleton ult)
      sufijos = map fst unida
   in reduce max 0 sufijos

{-
# Ejercicio 13
splitAtT :: Int -> TreeA a -> (TreeA a, TreeA a), que dado un natural n y una secuencia s, devuelva el par formado por los últimos n elementos de s y el resto (o sea, parte por el final en vez de por el principio).

Por ejemplo, si s = ⟨x0, x1, x2, x3, x4⟩:
splitAtT 2 s = (⟨x3, x4⟩, ⟨x0, x1, x2⟩)

Definir splitAtT con profundidad en O(h), donde h es la altura del árbol.
-}
data TreeA a = EA | NA Int (TreeA a) a (TreeA a)

splitAtT :: Int -> TreeA a -> (TreeA a, TreeA a)
splitAtT _ EA = (EA, EA)
splitAtT 0 arbol = (EA, arbol)
splitAtT n (NA t izq x der)
  | n <= sizeA der = 
    let (ultimos, resto) = splitAtT n der
     in (ultimos, NA (sizeA izq + sizeA resto + 1) izq x resto)
  | otherwise =
    let r = n - sizeA der - 1
        (ultIzq, restoIzq) = splitAtT r izq
     in (NA (sizeA der + 1 + sizeA ultIzq) ultIzq x der, restoIzq)

sizeA :: TreeA a -> Int
sizeA EA = 0
sizeA (NA t _ _ _) = t

{-
# Ejercicio 14
Se representan secuencias mediante árboles binarios:
data TreeB a = EB | NB Int (TreeB a) a (TreeB a), donde el Int guarda la longitud de la secuencia y el recorrido inorder da el orden de los elementos.

Definir en Haskell de manera eficiente
updateAt :: Int -> a -> TreeB a -> TreeB a, que dado un índice i, un valor v y una secuencia s, devuelva la secuencia resultante de reemplazar el elemento en la posición i por v. Si i no es un índice válido, devolver s sin cambios.

Por ejemplo, con s = ⟨10, 20, 30, 40, 50⟩:
updateAt 2 99 s = ⟨10, 20, 99, 40, 50⟩
updateAt 0 99 s = ⟨99, 20, 30, 40, 50⟩
updateAt 7 99 s = ⟨10, 20, 30, 40, 50⟩

Profundidad O(h).
-}
data TreeB a = EB | NB Int (TreeB a) a (TreeB a)

updateAt :: Int -> a -> TreeB a -> TreeB a
updateAt _ _ EB = EB
updateAt i val (NB t izq x der)
  | i < sizeB izq  = NB t (updateAt i val izq) x der
  | i == sizeB izq = NB t izq val der
  | otherwise      = NB t izq x (updateAt (i - sizeB izq - 1) val der)

sizeB :: TreeB a -> Int
sizeB EB = 0
sizeB (NB t _ _ _) = t

{-
# Ejercicio 15
El Int guarda la longitud de la secuencia, el inorder da el orden de los elementos.

Definir de manera eficiente:
countT :: (a -> Bool) -> TreeC a -> Int, que dado un predicado p y una secuencia s, devuelva cuántos elementos de s satisfacen p.

countT even ⟨3, 8, 5, 2, 7, 4⟩ = 3
countT (>10) ⟨3, 8, 5⟩ = 0

Profundidad O(h), trabajo O(n). Plantear las recurrencias.
-}
data TreeC a = EC | NC Int (TreeC a) a (TreeC a)

countT :: (a -> Bool) -> TreeC a -> Int
countT _ EC = 0
countT p (NC _ izq x der) =
  let (siIzq, siDer) = countT p izq ||| countT p der
   in if p x then siIzq + siDer + 1 else siIzq + siDer

(|||) :: a -> b -> (a, b)
a ||| b = (a, b)

{-
W(h) = 2·W(h-1) + c0 \in O(2^h) = O(n) con n = 2^h
S(h) = S(h-1) + c1 \in O(h)
-}

