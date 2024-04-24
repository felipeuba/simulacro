--problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
--  requiere: {True}
--  asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
--} 

reversoTupla :: (String, String) -> (String, String)
reversoTupla (a,b) = (b,a)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e (x:[]) = e == x
pertenece e (x:xs) = e == x || pertenece e xs

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x : []) = not(fst x == snd x)
relacionesValidas (x : xs) = not ((fst x == snd x) && (pertenece x xs) && (pertenece (reversoTupla x) xs)) && relacionesValidas(xs) 

--2

--problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--  requiere: {relacionesValidas(relaciones)}
--  asegura: {res no tiene elementos repetidos}
--  asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
--}

--[('a','b'), ('b','c')] = ['a','b','c']

--ultimo paso quitar todos (x:xs)

quitar :: (Eq t) => t -> [t] -> [t]
quitar e (x:[]) | e == x = []
                | otherwise = [x]
quitar e (x:xs) | e == x = xs
                | otherwise = [x] ++ (quitar e xs)

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos e (x:[]) | e == x = []
                     | otherwise = [x]
quitarTodos e (x:xs) | pertenece e (x:xs) =  quitarTodos e (quitar e (x:xs))
                     | otherwise = x : quitarTodos e (xs)


eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:[]) = [x]
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)


personasAux :: [(String, String)] -> [String]
personasAux [] = []
personasAux (x:[]) = eliminarRepetidos ([fst x] ++ [snd x])
personasAux (x:xs) = [fst x] ++ [snd x] ++ personas (xs)

personas :: [(String, String)] -> [String]
personas x = eliminarRepetidos (personasAux x)

--problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--  requiere: {relacionesValidas(relaciones)}
--  asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
--} 

--[("a","b"),("a","c")] = ["b","c"]

amigosDe :: String -> [(String, String)] -> [String]
amigosDe e [] = []
amigosDe e (x:[]) | e == fst x = [snd x]
                  | e == snd x = [fst x]
amigosDe e (x:xs) | e == fst x = [snd x] ++ amigosDe e xs
                  | e == snd x = [fst x] ++ amigosDe e xs
                  | otherwise = amigosDe e xs

