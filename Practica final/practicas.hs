-- =============================================================================
-- # Ejercicio 1
-- =============================================================================
data Trie = E | N String [Trie] deriving (Show)

-- =============================================================================
-- ### Inciso a
-- =============================================================================
isPrefix :: String -> String -> Bool
isPrefix "" _ = True
isPrefix s "" = False
isPrefix (x : xs) (y : ys)
  | x == y = isPrefix xs ys
  | otherwise = False

-- =============================================================================
-- ### Inciso b
-- =============================================================================
inv2 :: Trie -> Bool
inv2 E = True
inv2 (N s hijos) = and (map (okHijo s) hijos) && and (map inv2 hijos)

okHijo :: String -> Trie -> Bool
okHijo _ E = False
okHijo s (N s' _) = isPrefix s s'

-- =============================================================================
-- ### Inciso c
-- =============================================================================
inv3 :: Trie -> Bool
inv3 E = True
inv3 (N _ hijos) = hermanosOk hijos && and (map inv3 hijos)

difPref :: Trie -> Trie -> Bool
difPref E _ = False
difPref _ E = False
difPref (N s _) (N s' _) = not (isPrefix s s') && not (isPrefix s' s)

hermanosOk :: [Trie] -> Bool
hermanosOk [] = True
hermanosOk (t : ts) = and (map (difPref t) ts) && hermanosOk ts

-- =============================================================================
-- ### Inciso d
-- =============================================================================
isTrie :: Trie -> Bool
isTrie E = True
isTrie arbol = inv1 arbol && inv2 arbol && inv3 arbol

inv1 :: Trie -> Bool
inv1 E = True
inv1 (N s _) = s == ""

-- =============================================================================
-- ### Inciso e
-- =============================================================================
prefixes :: String -> Trie -> [String]
prefixes _ E = []
prefixes s t@(N s' hijos)
  | isPrefix s s' = words' t
  | otherwise = concat (map (prefixes s) (filter (bajar s) hijos))

bajar :: String -> Trie -> Bool
bajar _ E = False
bajar s (N s' _) = isPrefix s' s

words' :: Trie -> [String]
words' E = []
words' (N s hijos) = s : concat (map words' hijos)