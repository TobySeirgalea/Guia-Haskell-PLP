--Dar el tipo y describir el comportamiento de las siguientes funciones
--null :: [a] -> Bool 
--Recibe una lista y retorna si está vacía

--head :: [a] -> a
--Recibe una lista y retorna su cabeza

--tail :: [a] -> [a]
--Recibe una lista y retorna su cola

--init :: [a] -> [a]
--Recibe una lista no vacía y retorna una lista con todos sus elementos menos el último

--last :: [a] -> a
--Recibe una lista y retorna su ultimo elemento

--take :: Int -> [a] -> [a]
--Recibe un entero k y una lista y retorna los primeros k elementos de la lista

--drop :: Int -> [a] -> [a]
--Recibe un entero k y una lista, retorna la lista sin los primeros k elementos

--(++) :: [a] -> [a] -> [a]
--Recibe dos listas y las concatena

--concat :: [[a]] -> [a]
--Recibe una lista de listas y retorna la lista resultante de concatenarlas 

--reverse :: [a] -> [a]
--Recibe una lista y retorna la misma lista en reversa

--elem :: a -> [a] -> Bool 
--Recibe un elemento y una lista y retorna un booleano que indica que si ese elemento pertenece a la lista 

valorAbsoluto :: Float -> Float
valorAbsoluto n | n >= 0 = n
                | otherwise = -n

bisiesto :: Int -> Bool
bisiesto n = n `mod` 4 == 0 && not (n `mod` 100 == 0 || n `mod` 400 == 0)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

tieneAlgunDivisorNoTrivialAPartirDe :: Int -> Int -> Bool
tieneAlgunDivisorNoTrivialAPartirDe n i
                                    |i == n = False
                                    |n `mod` i == 0 = True
                                    |otherwise = tieneAlgunDivisorNoTrivialAPartirDe n (i+1)

esPrimo :: Int -> Bool
esPrimo n = n > 1 && not (tieneAlgunDivisorNoTrivialAPartirDe n 2)

--Dado un número devuelve su inverso multiplicativo si está definido, o Nothing en caso contrario.
inverso :: Float -> Maybe Float
inverso n |n == 0 = Nothing
          |otherwise = Just (1 / n)
--Convierte a entero una expresión que puede ser booleanao entera. En el caso de los booleanos, el entero que corresponde es 0 para False y 1 para True
aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right n) = if n then 1 else 0

eliminarAparicionesDelCaracter :: Char -> String -> String
eliminarAparicionesDelCaracter char [] = []
eliminarAparicionesDelCaracter char (y:ys) |char == y = eliminarAparicionesDelCaracter char ys
                                           |otherwise = y:eliminarAparicionesDelCaracter char ys

--Elimina to das las apariciones de cualquier carácter de la primera cadena en la segunda. Por ejemplo, limpiar ``susto'' ``puerta'' evalúa a ``pera''.Nota: String es un renombre de [Char]. La notación ``hola'' es equivalente a [`h',`o',`l',`a'] y a `h':`o':`l':`a':[].
--Idea: Recorremos ys y nos fijamos para cada caracter si está en xs, si es así -> lo sacamos, sino lo dejamos
limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar _ [] = []
limpiar xs (y:ys) |y `elem` xs = limpiar xs ys
                  |otherwise = y:limpiar xs ys
--Dada una lista de números devuelve la diferencia de cada uno con el promedio general. Por ejemplo, difPromedio [2, 3, 4] evalúa a [-1, 0, 1]
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio (x:xs) =  sumarALista (-promedioGeneral (x:xs)) (x:xs)

promedioGeneral :: [Float] -> Float
promedioGeneral [] = 0
promedioGeneral (x:xs) = sumarLista (x:xs) / fromIntegral (length (x:xs))

sumarLista :: [Float] -> Float
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

sumarALista :: Float -> [Float] -> [Float]
sumarALista _ [] = []
sumarALista n (x:xs) = n + x : sumarALista n xs

--Indica si una lista de enteros tiene todos sus elementos iguales.
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:ys) = x == y && todosIguales (y:ys)

data AB a = Nil | Bin (AB a) a (AB a) deriving Show

--Indica si un árbol es vacío (i.e. no tiene nodos).
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

--Dado un árbol de booleanos construye otro formado por la negación de cada uno de los nodos.
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i c d) = Bin (negacionAB i) (not c) (negacionAB d)

--Calcula el producto de todos los nodos del árbol.
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i c d) = c * productoAB i * productoAB d
------------------------------------------------------------------
------------------------------Guía 1------------------------------
------------------------------------------------------------------
{-Ejercicio 1:-}
{- ¿Cuál es el tipo de cada función? (Suponer que todos los números son de tipo Float).
Indicar cuáles de las funciones anteriores no están currificadas. Para cada una de ellas, definir la función
currificada correspondiente. Recordar dar el tipo de la función.-}
max2 :: (Float, Float) -> Float --No está currificada
max2 (x, y) | x >= y = x
            | otherwise = y
max2Currificada :: Float -> Float -> Float
max2Currificada x y | x >= y = x
                    | otherwise = y

normaVectorial :: (Float, Float) -> Float --No está currificada
normaVectorial (x, y) = sqrt (x^2 + y^2)
normaVectorialCurrificada :: Float -> Float -> Float
normaVectorialCurrificada x y = sqrt (x^2 + y^2)

subtract' :: Float -> Float -> Float --Está currificada 
subtract' = flip (-)

predecesor :: Float -> Float --Está currificada
predecesor = subtract' 1

evaluarEnCero :: (Float -> Float) -> Float --Está currificada
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> (a -> a) --Está currificada
dosVeces = \f -> f . f

{- Tipo de map = (a -> b) -> ([a] -> [b]), tipo de flip = (A -> B -> C) -> (B -> (A -> C)) 
 map va a recibir a flip como función, entonces el tipo de flip es igual a (a -> b)
 Entonces a -> b = (A -> B -> C) -> (B -> (A -> C))
 a = A -> B -> C y b = B -> A -> C
 Reemplazo y queda map flip :: [A -> B -> C] -> [B -> A -> C]-}
flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

{- El tipo de flip es (a -> b -> c) -> (b -> (a -> c))
 En este caso es flip la que cumple el rol de (a -> b -> c)
 Por lo tanto (a -> (b -> c)) = (A -> B -> C) -> (B -> (A -> C))
 a = A -> B -> C y b = B y c = A -> C
 Quedando flipRaro :: B -> ((A -> B -> C) -> A -> C) -}
flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

{-Ejercicio 2:-}
-- Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

-- Definir la función uncurry, que dada una función currificada de dos argumentos, devuelve su versión no currificada equivalente. Es la inversa de la anterior.
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y devuelva su versión currificada?
-- Sugerencia: pensar cuál sería el tipo de la función

--CONSULTAR

{-Ejercicio 3:-}
--Redefinir usando foldr las funciones sum, elem, (++), filter y map.
sum' :: [Int] -> Int
sum' = foldr (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' elemento = foldr (\x rec -> x == elemento || rec) False

(++.) :: [a] -> [a] -> [a]
(++.) [] xs = xs
(++.) (y:ys) xs = y: (ys ++. xs)

(++-) :: [a] -> [a] -> [a]
(++-) ys xs = foldr (\m rec -> m:rec) xs ys
--En correccion usamos ys como parametro sobre el que hacemos recursion

-- (++-) xs = foldr (\m rec -> m:rec ) (xs)  
-- [1,2,3] (++-) [4,5]
-- (++-) [1,2,3] [4,5]
-- foldr (\m rec -> m:rec ) ([1,2,3]) [4,5]
-- foldr (\4 rec -> 4:rec ) ([1,2,3]) [5]
-- 4:foldr (\m rec -> m:rec ) ([1,2,3]) [5]
-- 4:foldr (\5 rec -> 5:rec ) ([1,2,3]) []
-- 4:5:[1,2,3] 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x rec -> if f x then x:rec else rec) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x rec -> f x : rec) []

{-Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún (>).-}
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

{-Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ;[1,5,4,4,9] -}
sumasParcialesError :: Num a => [a] -> [a]
sumasParcialesError xs = foldl (\acc x -> if null acc then x:acc else (sum(acc)+x):xs ) [] xs
{-
sumasParciales [1,4,-1,0,5] = foldl (\acc x -> if null acc then x:acc else (sum(acc)+x):[1,4,-1,0,5] ) [] [1,4,-1,0,5]
foldl (\[] x -> if null acc then [x] else (sum(acc)+x):[1,4,-1,0,5] ) [] [1,4,-1,0,5]
2) foldl (\[1] 4 -> (sum([1])+4):[1,4,-1,0,5] ) [-1,0,5]
foldl (\[5,1,4,-1,0,5] -1 -> (sum([5,1,4,-1,0,5])-1):[1,4,-1,0,5] ) [0,5]
foldl (\[13,1,4,-1,0,5] 0 -> (sum([13,1,4,-1,0,5])+0):[1,4,-1,0,5] ) [5]
foldl (\[22,1,4,-1,0,5] 5 -> (sum([22,1,4,-1,0,5])+5):[1,4,-1,0,5] ) []
[36,1,4,-1,0,5]
Lo que quiero hacer es en vez de sumar todo el acumulado solo tomar el ultimo ya que ese es el valor de la suma acumulada
ej: [1,4,-1,0,5] -> [1,4,-1,0,5] -> [1,1+4,-1,0,5] -> [1,5,5-1,0,5] -> [1,5,4,0,5] -> [1,5,4,4,4+5] -> [1,5,4,4,9]
-}
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\acc x -> if null acc then [x] else acc ++ [last acc + x]) []

{-Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.-}
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x rec -> x-rec) 0 --veo a x como primero y rec como segundo
--Podemos verlo así: foldr f [1,2,3,4,5,6] = f 1 (f 2 (f 3 (f 4 (f 5(f 6))))) si f = (-) -> 1-(2-(3-(4-(5-(6-0)))))
--1-(2-(3-(4-(5-6)))) = 1-(2-(3-5)) = 1-(2-(-2)) = 1-(4) = - 3

{-Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso.
-}
sumaAltEnOrdenInverso :: Num a => [a] -> a
sumaAltEnOrdenInverso = foldl (\acc x -> -acc + x) 0 --veo a acc como anteultimo y a x como ultimo

{-Ejercicio 4:-}
{-Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutacio-
nes. Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop.-}
permutaciones :: [a] -> [[a]]  --concatMap a cada elemento de lista le aplica f  y retorna una lista con todos los resultados concatenados
permutaciones [x] = [[x]]
permutaciones (x:xs) =  concatMap (insertarEnTodasPartes' x) (permutaciones xs)
--Quiero insertar en todas partes x en permutaciones xs.¿Cómo hago? permutaciones [2,3] = [[2,3],[3,2]] y yo quiero
--insertarEnTodasPartes' 1 en cada una y retornalo en una lista. Es decir aplicar una función a cada lista de la lista grande y retornar una lista con todos los resultados
--Esto lo hace concatMap, pero para que insertarEnTodasPartes' sea una función de a -> [b] necesito que esté parcialmente aplicada con 1, así queda:
-- a' = [a], [b'] = [[a]] entonces b' = [a] concatMap :: ([a] -> [[a]]) -> [[a]] -> [[a]]  
{-Supongamos que tenemos el recursivo resuelto, ¿cómo generamos todas las permutaciones con eso y x?
ej: permutaciones [1,2,3] tengo ya resuelto permutaciones [3,2] = [3,2], [2,3]
¿Qué tengo que hacer con 1 y [3,2], 1 y [2,3] para tener todas las permutaciones
Si meto el 1 en todos los lugares posibles y los guardo tengo de 1 y [3,2] -> [1,3,2],[3,1,2],[3,2,1]
de 1 y [2,3] -> [1,2,3],[2,1,3],[2,3,1] juntandolas tengo todas las permutaciones, para esto puedo usar concatMap
-}
insertar :: a -> Int -> [a] -> [a]
insertar x i xs = (take i xs)++x:(drop i xs) 

insertarEnTodasPartesHasta :: Int -> a -> [a] -> [[a]]
insertarEnTodasPartesHasta n x xs |n >= 0 = insertar x n xs : insertarEnTodasPartesHasta (n-1) x xs 
                                  |otherwise = []
--Otra alternativa por si no se puede usar listas por compresión
insertarEnTodasPartes'' :: a -> [a] -> [[a]]
insertarEnTodasPartes'' x xs = insertarEnTodasPartesHasta (length xs) x xs
--Como n siempre queremos que se mueva entre [0,length xs] podemos hacerlo directo así:
insertarEnTodasPartes' :: a -> [a] -> [[a]]
insertarEnTodasPartes' x xs = [insertar x i xs | i <- [0..length xs]] 
--Ahora hay que pasar permutaciones de recursion 
permutaciones' :: [a] -> [[a]]
permutaciones' = foldr (\x rec -> concatMap (insertarEnTodasPartes'' x) rec) [[]]


{-Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
mismos elementos de L, en su mismo orden de aparición.
Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
(en algún orden)-}
partesExplicita :: [a] -> [[a]]
partesExplicita = foldr (agregarATodas2) [[]] 
{-
agregarATodas 5 (agregarATodas 2 (agregarATodas 1 []))
-}

--Supongo que tengo la cola resuelta. ¿Cómo hago con x para tener partes de (x:xs)
--Ej: partes [1, 2] = [[1],[2],[1,2]] 
-- Agrego [5], [5,1] [5,2] [5,1,2] quiero agregar 5 a todos elementos de la recursión, pero sin perder los que tengo
agregarATodas :: a -> [[a]] -> [[a]]
agregarATodas y xs = foldr (\x rec -> [y:x] ++ rec++xs) xs xs

--Funciona bien
agregarATodas2:: a -> [[a]] -> [[a]]
agregarATodas2 _ [] = []
agregarATodas2 elem (x:xs) = [elem:x] ++ [x] ++ agregarATodas2 elem xs

--En recursión implícita:
agregarATodasV2 :: a -> [[a]] -> [[a]]
agregarATodasV2 elem = foldr (\x rec -> [elem:x] ++ [x] ++ rec) []

partes :: [a] -> [[a]]
partes = foldr (agregarATodasV2) [[]]

{-Definir la función prefijos, que dada una lista, devuelve todos sus prefijos.
Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]-}
prefijos :: [a] -> [[a]]
prefijos = foldl (\acc x -> ((head acc) ++ [x] ):acc ) [[]]

prefijosE :: [a] -> [[a]]
prefijosE [] = [[]]
prefijosE (x:xs) = agregarAdelanteATodasLasQueEstan x (prefijosE xs)

prefijosConFoldr :: [a] -> [[a]]
prefijosConFoldr = foldr (agregarAdelanteATodasLasQueEstan) [[]]
--Queda bien porque la proxima cabeza que tomo la pongo adelante de todos los resultados recursivos

agregarAdelanteATodasLasQueEstan :: a -> [[a]] -> [[a]]
agregarAdelanteATodasLasQueEstan y = foldr (\x rec -> [y:x] ++ rec) [[]]
--predfijos [2] = [[2],[]] prefijos [1,2] = [1,2],[1] prefijos [5,1,2] = [5,1,2], [5,1], [5]
--Voy recorriendo hacia adelante la lista, en cada paso tomo la cabeza acumulada, le agrego el elemento actual al final y la pongo adelante de mi acumulado
--Así en proxima es a esa misma lista a la que le agrego al final el siguiente elemento
sufijos :: [a] -> [[a]]
sufijos [] = [[]]
sufijos (x:xs) = (x: head(sufijos xs)) : sufijos xs  
--Voy recorriendo de atras hacia adelante, acumulando en rec. 
--sufijos [1,2] -> [[1,2],[2]]. Para obtener sufijos [0,1,2] = [[0,1,2] [1,2] [2]] debo agregar x a la cabeza de la recursion
sufijos' :: [a] -> [[a]]
sufijos' = foldr (\x rec -> (x:head(rec)) : rec) [[]]

-- f 5 (f 1 (f 2 [])) -> 
-- f 2 (f 1 (f 5 [])) ->  

--prefijosE [1,2] = [[], [1], [1,2]] -> prefijosE [5,1,2] = agregar a todas 5 
--prefijosE [] = [[]] -> prefijosE [2] = [[2],[]] -> prefijosE [1,2] = [[1,2],[2],[]]

{-Definir la función sublistas que, dada una lista, devuelve to das sus sublistas (listas de elementos que
aparecen consecutivos en la lista original).
Ejemplo: sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]]
(en algún orden).-}
--OBS!: prefijos de cada uno de los sufijos = sublistas = sufijo de cada uno de los prefijos
sublistas1 :: [a] -> [[a]]
sublistas1 xs = filter (not.null) (concatMap sufijos' (prefijos xs)) 
--Si quiero aplicar una función a todas las listas de una lista y recibir una lista con todos los resultados de aplicar esa funcion a cada lista concatenados USO concatMap
sublistas2 :: [a] -> [[a]]
sublistas2 xs = filter (not.null) (concatMap prefijos (sufijos' xs)) 

{-Ejercicio 5-}
{-Considerar las siguientes funciones:-}
{-Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
En caso contrario, explicar el motivo.-}
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)
--No es recursión estructural ya que utiliza el acceso a la cola sin recursión aplicada sobre ella. Es recursión primitiva

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)
{-El caso para la lista no vacía es una reescritura de:
entrelazar [] = (\ys -> ys) --esto es lo mismo que entrelazar [] ys = ys 
entrelazar (x:xs) ys = if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys) 
    Por lo tanto vemos que el parámetro sobre el que se hace recursión es la primer lista. 
    Teniendo esto en cuenta notar que solo accedemos a xs cuando es el resultado de la recursion. Es estructural
-}
entrelazarConFoldr :: [a] -> [a] -> [a]
entrelazarConFoldr = foldr (\x rec ys -> if null ys then x : (rec []) else x : head ys : rec (tail ys)) id 
{-foldr :: (a -> b -> b) -> b -> [a] -> b
si la lambda es (\x rec ys -> ...) x::a , rec ys :: b por lo que rec :: b -> b y ys :: b -}