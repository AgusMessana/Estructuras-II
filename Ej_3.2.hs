data Linea = Linea [Int] Int [Int] deriving Show

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

insertar :: Int -> Linea -> Linea
insertar x (Linea [] pos der) = Linea [x] (pos + 1) der
insertar x (Linea izq pos der) = Linea (izq ++ [x]) (pos + 1) der

borrar :: Linea -> Linea
borrar (Linea [] pos der) = Linea [] pos der
borrar (Linea izq pos der) = Linea (init izq) (pos - 1) der