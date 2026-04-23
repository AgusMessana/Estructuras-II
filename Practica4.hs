-- 1)a)
data Tree a = Leaf | Node (Tree a) a (Tree a)

completo :: a -> Int -> Tree a
compelto x 0 = Leaf
completo x d = Node t x t
    where t = completo x (d-1)

-- 2)
data BST a = Hoja | Nodo (BST a) a (BST a)

-- a)
maximum' :: Ord a => BST a -> a
maximum' (Nodo _ x Hoja) = x
maximum' (Nodo _ _ der) = maximum' der

-- b)
checkBST :: Ord a => BST a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja x Hoja) = True
checkBST (Nodo Hoja x der) = x < minimum' der && checkBST der 
checkBST (Nodo izq x Hoja) = maximum' izq <= x && checkBST izq
checkBST (Nodo izq x der) = maximum' izq <= x && minimum' der > x && checkBST izq && checkBST der

minimum' :: Ord a => BST a -> a
minimum' (Nodo Hoja x _) = x
minimum' (Nodo izq _ _) = minimum' izq

-- c)
splitBST :: Ord a => BST a -> a -> (BST a, BST a)
splitBST Hoja x = (Hoja, Hoja)
splitBST (Nodo izq y der) x
    | x == y = (Nodo izq y Hoja, der)
    | x < y = let (izq', der') = splitBST izq x
               in (izq', Nodo izq' y der)
    | x > y = let (izq', der') = splitBST der x
               in (Nodo izq y der', der')

-- con paralelizacion dedinir un operador de paralelelismo a ||| b = (a, b)