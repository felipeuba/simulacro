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


import random

def poner_pos_pares_en_cero (s:list[int]) -> list[int]:
    res:list[int] = s.copy()
    for i in range(0,len(s),1):
        if i % 2 == 0:
            res[i] = 0
    return res

def solicitar_alumnos () -> list[str]:
    res:list[str] = []
    siga_ingresando:bool = True
    while siga_ingresando:
        nombre = input("Ingrese el nombre del alumno")
        if nombre != "listo":
            res.append(nombre)
        else:
            siga_ingresando = False
    return res



def monedero_electronico () -> list[tuple[str,float]]:
    res:list [list[str,float]] = []
    siga_ingresando:bool = True
    while siga_ingresando:
        opcion = input("Ingrese C para cargar, D para descontar o X para salir \n")
        if opcion == "C":
            res.append(("C",float(input("Cuanto quiere cargar \n"))))
        elif opcion == "D":
            res.append(("D",float(input("Cuanto quiere descontar \n"))))
        elif opcion == "X":
            siga_ingresando = False
        else:
            print("ingresa lo que te digo flaco")
    return res

#print(monedero_electronico())

cartas:list[float] = [1,2,3,4,5,6,7,10,11,12]
repartir:float = random.choice(cartas)
def siete_y_medio_aux (num:int) -> float:
    if num < 8:
        return num
    else:
        return 0.5

def siete_y_medio () -> list[int]:
    puntuacion:float = 0
    seguir:bool = True
    while seguir:
        opcion:str = input("quiere seguir jugando si/no \n")
        if opcion == "si":
            puntuacion += siete_y_medio_aux(repartir)
            if puntuacion >= 7.5:
                seguir = False
        elif opcion == "no":
            seguir = False
    if puntuacion == 7.5:
        return f"Ganaste: {puntuacion}"
    else:
    return f"Perdiste: {puntuacion}"

print(siete_y_medio())

