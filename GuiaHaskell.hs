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
sumarLista = sum

sumarALista :: Float -> [Float] -> [Float]
sumarALista _ [] = []
sumarALista n (x:xs) = n + x : sumarALista n xs

--Indica si una lista de enteros tiene todos sus elementos iguales.
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:ys) = x == y && todosIguales (y:ys)

data AB'' a = Nil' | Bin' (AB'' a) a (AB'' a) --deriving Show

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
evaluarEnCero f = f 0

dosVeces :: (a -> a) -> (a -> a) --Está currificada
dosVeces f = f . f

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
sum' = sum

elem' :: (Eq a) => a -> [a] -> Bool
elem' elemento = foldr (\x rec -> x == elemento || rec) False

(++.) :: [a] -> [a] -> [a]
(++.) [] xs = xs
(++.) (y:ys) xs = y: (ys ++. xs)

(++-) :: [a] -> [a] -> [a]
(++-) ys xs = foldr (:) xs ys
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
map' = map

{-Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún (>).-}
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

{-Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ;[1,5,4,4,9] -}
sumasParcialesError :: Num a => [a] -> [a]
sumasParcialesError xs = foldl (\acc x -> if null acc then x:acc else (sum acc+x):xs ) [] xs
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
sumaAlt = foldr (-) 0 --veo a x como primero y rec como segundo
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
insertar x i xs = take i xs++x:drop i xs

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
permutaciones' = foldr (concatMap . insertarEnTodasPartes'') [[]]


{-Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
mismos elementos de L, en su mismo orden de aparición.
Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
(en algún orden)-}
partesExplicita :: [a] -> [[a]]
partesExplicita = foldr agregarATodas2 [[]]
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
partes = foldr agregarATodasV2 [[]]

{-Definir la función prefijos, que dada una lista, devuelve todos sus prefijos.
Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]-}
prefijos :: [a] -> [[a]]
prefijos = foldl (\acc x -> (head acc ++ [x] ):acc ) [[]]

prefijosE :: [a] -> [[a]]
prefijosE = foldr agregarAdelanteATodasLasQueEstan [[]]

prefijosConFoldr :: [a] -> [[a]]
prefijosConFoldr = foldr agregarAdelanteATodasLasQueEstan [[]]
--Queda bien porque la proxima cabeza que tomo la pongo adelante de todos los resultados recursivos

agregarAdelanteATodasLasQueEstan :: a -> [[a]] -> [[a]]
agregarAdelanteATodasLasQueEstan y = foldr (\x rec -> (y:x) : rec) [[]]
--predfijos [2] = [[2],[]] prefijos [1,2] = [1,2],[1] prefijos [5,1,2] = [5,1,2], [5,1], [5]
--Voy recorriendo hacia adelante la lista, en cada paso tomo la cabeza acumulada, le agrego el elemento actual al final y la pongo adelante de mi acumulado
--Así en proxima es a esa misma lista a la que le agrego al final el siguiente elemento
sufijos :: [a] -> [[a]]
sufijos [] = [[]]
sufijos (x:xs) = (x: head (sufijos xs)) : sufijos xs
--Voy recorriendo de atras hacia adelante, acumulando en rec. 
--sufijos [1,2] -> [[1,2],[2]]. Para obtener sufijos [0,1,2] = [[0,1,2] [1,2] [2]] debo agregar x a la cabeza de la recursion
sufijos' :: [a] -> [[a]]
sufijos' = foldr (\x rec -> (x:head rec) : rec) [[]]

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
sublistas1 xs = concatMap (filter (not.null) . sufijos') (prefijos xs)
--Si quiero aplicar una función a todas las listas de una lista y recibir una lista con todos los resultados de aplicar esa funcion a cada lista concatenados USO concatMap
sublistas2 :: [a] -> [[a]]
sublistas2 xs = concatMap (filter (not.null) . prefijos) (sufijos' xs)

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
entrelazarConFoldr = foldr (\x rec ys -> if null ys then x : rec [] else x : head ys : rec (tail ys)) id
{-foldr :: (a -> b -> b) -> b -> [a] -> b entonces cuando ponemos 3 parámetros en la lambda queda x::a, rec ys::b por lo tanto rec :: b -> b, ys::b 
por eso en la lambda simpre luego de rec paso una lista
-}
{-Ejercicio 6-}
{-Esquema de recursión primitiva sobre listas-}
--Función recibe cabeza, cola de lista y recursión sobre cola y retorna tipo salida
--Caso base es de tipo salida
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f b [] = b
recr f b (x:xs) = f x xs (recr f b xs)
--x::a, xs::[a], (recr f b xs) :: b
{-Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
resultado de eliminar de la lista la primera aparición del elemento (si está presente).-}
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna elem = recr (\x xs rec -> if x == elem then xs else x:rec) []
--Si encontraste el elemento, retorna la cola, así solo sacás su primera aparición
--Si no es el elemento entonces agregalo a la recursión sobre la cola. Así no sacás nada que no sea la primera aparición de elemento

{-Explicar p or qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función
sacarUna del punto anterior
RTA: No es adecuado porque necesito tener acceso a la lista sin la recursión aplicada sobre ella, ya que quiero solo sacar la primera aparición de elemento.
Si no tengo acceso a la cola sin la recursión entonces sacaría todas las apariciones-}

{-Definir la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
ordenada (de manera creciente), de manera que se preserva el ordenamiento.
-}
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem = recr (\x xs rec -> if x < elem && elem < head xs then x:elem:xs else x:rec ) [elem]
--Falla con 0 [1,2,3,4,5,6,7,8,9] -> [1,2,3,4,5,6,7,8,9,0] porque entro siempre en else 

insertarOrdenadoV2 :: Ord a => a -> [a] -> [a]
insertarOrdenadoV2 elem = recr (\x xs listaConElemInsertadoOrdenado -> if elem > x then x:listaConElemInsertadoOrdenado else elem:x:xs) [elem]
--Si es mayor que el actual, entonces meto el actual delante de la lista con el elemento insertado ordenado, sino entonces es menor y debe estar antes que el actual.

{-Ejercicio 7-}
{-Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos, 
a partir de un elemento inicial y de una función de incremento entre los elementos de la lista. 
Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente.-}
-- a es el elemento inicial, (a->a) es la función incremento, Integer la cantidad de elementos, [a] tipo retorno
genLista :: a -> (a -> a) -> Integer -> [a]
genLista _ _ 0 = []
genLista init incr size = init:genLista (incr init) incr (size-1)

genListFoldNN :: Integer -> a -> (a -> a) -> [a]
genListFoldNN 0 = \init incr -> []
genListFoldNN n = \init incr -> init:genListFoldNN (n-1) (incr init) incr

genListFoldN :: Integer -> a -> (a -> a) -> [a]
genListFoldN n init incr = foldNat (\x rec -> rec ++ [incr (last rec)]) [init] (n-1)
--Le pongo n-1 a la derecha para contrarrestar que foldNat va hasta cero

genListaFoldr :: a -> (a -> a) -> Integer -> [a]
genListaFoldr init incr size = foldr (\x rec -> if size == toInteger (length rec) then rec else incr (head rec):rec) [init] (take (fromIntegral size) [1..])

genListaFoldrOrden :: a -> (a -> a) -> Integer -> [a]
genListaFoldrOrden init incr size = foldr (\x rec -> if size == toInteger (length rec) then rec else rec ++ [(incr.last) rec]) [init] (take (fromIntegral size) [1..])
{-
genListaFoldr init incr size = foldr (\x rec -> if size == toInteger(length rec) then rec else incr(head rec):rec) [init] [init] 1 (+2) 4
foldr (\1 rec -> if 4 == toInteger(length rec) then rec else (+2)(head rec):rec) [1] [1]
rec = [init] Entonces necesito pasarle a foldr una lista de tamaño size 
-}
genList :: a -> (a -> a) -> Integer -> [a]
genList init f size = foldl (\acc x -> if toInteger (length acc) == size then acc else acc ++ [f (last acc)]) [init] (take (fromIntegral size) [1..])

{-Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el
segundo), devuelve una lista de números consecutivos desde el primero hasta el segundo.-}
desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta d h = genList d (+1) (h-d+1)

{-Ejercicio 8-}
{-Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
listas finitas e infinitas.
1) mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry
-}
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

{-2)armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial. -}
armarPares :: [a] -> [b] -> [(a,b)]
armarPares = foldr (\x rec ys -> if null ys then [] else (x, head ys):rec (tail ys)) (const [])

{-3) mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
(de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento corresp ondiente de
las dos listas. Esta función en Haskell se llama zipWith. -}
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = map (uncurry f) (armarPares xs ys)

{-Ejercicio 9-}
{-Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
to das de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como
la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
dimensiones.
-}
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\x rec ys -> zipWith (+) x (head ys) : rec (tail ys)) (const [])
{-zipWith (+) recibe dos listas (filas de matriz) suma celda a celda y retorna los resultados en una lista.
Si a cada lista le hacemos zipWith (+) con la de la otra matriz y agregamos el resultado a la lista de listas obtenida de hacer recursión sobre la siguiente filas de ambas matrices
-}

{-2).Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, todas de longitud M, la salida debe tener M listas, todas de longitud N.
trasponer :: [[Int]] -> [[Int]]-}
trasponer :: [[Int]] -> [[Int]]
trasponer = foldr insertarEnCadaColumna []
{-
[[1,2,3],  [[1,4],
 [4,5,6]]   [2,5],        
            [3,6]]
Asumiendo que tenemos [[4],[5],[6]] ¿Qué debo hacer con [1,2,3] para tener la traspuesta?. Debo agregar cada elemento al principio de una de las columnas
-}
--Con recursión explícita
insertarAlInicioDeCada :: [Int] -> [[Int]] -> [[Int]]
insertarAlInicioDeCada xs [] = []
insertarAlInicioDeCada [] yss = yss
insertarAlInicioDeCada (x:xs) yss = (x: head yss) : insertarAlInicioDeCada xs (tail yss)
--Con foldr
insertarEnCadaColumna :: [Int] -> [[Int]] -> [[Int]]
insertarEnCadaColumna = recr (\x xs rec yss -> if (not.null) yss then (x: head yss) : rec (tail yss) else insertarComoCols (x:xs) ) (const [])
--OBS! como en recursión explícita usamos un patrón para yss == [], acá en el fold debemos agregarlo con if then else
--Si no hay más columnas debo hacer que los elementos de la fila que quiero poner se conviertan en columnas para que sea el caso base. Eso lo hace insertar como cols    
insertarComoCols :: [Int] -> [[Int]]
insertarComoCols = map ((: []))

{-Ejercicio 10-}
{-a) Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
Haskell (la función va a estar definida sólo para los enteros mayores o iguales que 0).-}
-- foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat casoRec casoBase n = case n of
                            0 -> casoBase
                            n -> casoRec n (rec (n-1))
                            where rec = foldNat casoRec casoBase

{-b) Utilizando foldNat, definir la función potencia.-}
potencia :: Integer ->  Integer
potencia = foldNat (*) 1 --esto es factorial
--Quiero hacer recursión sobre n, entonces n debe ser el exponente
--pot :: Integer -> Integer -> Integer
{-
pot 2 3 = 2 * pot 2 2 = 2*2*pot 2 1 = 2*2*2*pot 2 0 = 2*2*2*1 = 8
-}
potenciaExplicita :: Integer -> Integer -> Integer
potenciaExplicita _ 0 = 1
potenciaExplicita base exp = base * potenciaExplicita base (exp-1)
--Lo pasamos a foldNat
potenciaV2 :: Integer -> Integer -> Integer
potenciaV2 base = foldNat (\exp rec -> base*rec) 1
--En foldNat, nuestra lambda tiene (\decrementador resultado recursion)
--Por eso foldNat (*) 1 = foldNat (\dec rec -> dec * rec) 1 = n*(n-1)*...*1 que es factorial. En cambio en potencia queremos hacer base* base exp veces

{-Ejercicio 11-}
{-Definir el esquema de recursión estructural para el siguiente tipo:-}
data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)
{-Luego usar el esquema definido para escribir la función evaluar :: Num a => a -> Polinomio a -> a
que, dado un número y un polinomio, devuelve el resultado de evaluar el polinomio dado en el número dado.-}

{-¿Cómo lo hacemos?: Vemos los tipos de sus constructores
X :: Polinomio a                                                                   ---Constructor base
Cte a :: a -> Polinomio a                                                          ---Constructor base
Suma (Polinomio a) (Polinomio a) :: Polinomio a -> Polinomio a -> Polinomio a      ---Constructor recursivo
Prod (Polinomio a) (Polinomio a) :: Polinomio a -> Polinomio a -> Polinomio a      ---Constructor recursivo
El tipo de foldPolinomio será :: (b -> b -> b) -> (b -> b -> b) -> b -> (a -> b) -> Polinomio a -> b
                                casoSuma      casoProd   casoX   casoCte    Estructura    tipoRetorno
-}
foldPolinomio :: (b-> b -> b) -> (b-> b -> b) -> b -> (a -> b) -> Polinomio a -> b
foldPolinomio casoSuma casoProd casoX casoCte poli = case poli of
                                                    X -> casoX
                                                    Cte n -> casoCte n --Si tiene parámetros se los paso
                                                    Suma p q -> casoSuma (rec p) (rec q) --Si es recursivo aplico la función pero sobre resultados recursión sus parámetros
                                                    Prod p q -> casoProd (rec p) (rec q)
                                                    where rec = foldPolinomio casoSuma casoProd casoX casoCte
evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPolinomio (+) (*) n id
--Se define qué se quiere hacer en cada caso con cada constructor posible. Si hay una suma quiero sumar los resultados de evaluar cada polinomio de la suma, lo mismo con producto, si es una variable la quiero reemplazar por n y si es una cte quiero que sea la misma
polinomio1 = Suma (Prod (Suma X (Cte 3)) X ) (Prod (Suma X (Cte (-11))) X) -- X*(3+X) + (-11 + X)*X = 3X + X^2 - 11X + X^2 = 2X^2 - 8X

{-Ejercicio 12-}
{-Considerar el siguiente tipo, que representa a los árboles binarios:-}
data AB a = Nil | Bin (AB a) a (AB a) --deriving Show
{-a) Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y
dar sus tipos.-}
-- Nil:: AB a , Bin :: AB a -> a -> AB a -> AB a
--(recSobreIzq -> raiz -> recSobreDer -> salida) -> salidaCasoBase -> Estructura -> salida
foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB casoBin casoNil arbol = case arbol of
                               Bin i r d -> casoBin (rec i) r (rec d)
                               Nil -> casoNil
                            where rec = foldAB casoBin casoNil
--El orden en que declaré los parámetros en el tipo lo debo respetar al poner los parámetros en casoBin
recrAB :: (b -> a -> b -> AB a -> AB a -> b) -> b -> AB a -> b
recrAB casoBin casoNil arbol = case arbol of
                                Nil -> casoNil
                                Bin i r d -> casoBin (rec i) r (rec d) i d
                            where rec = recrAB casoBin casoNil
{-b) Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB
o recAB).-}
esNil :: AB a -> Bool
esNil arbol = case arbol of
            Nil -> True
            _ -> False
altura :: AB a -> Int
altura = foldAB (\recIzq r recDer -> 1 + max recDer recIzq) 1

cantNodos :: AB a -> Int
cantNodos = foldAB (\recIzq r recDer -> recIzq + 1 + recDer) 0

{-c) Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión
para un árbol que puede o no ser Nil.-}
mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f Nil = error "No se puede obtener el mejor según ningún criterio en un arbol Nil"
mejorSegún f (Bin i r d) = foldAB (\recIzq r recDer -> compararYDarMejor f recDer (compararYDarMejor f r recIzq)) r (Bin i r d)
        where compararYDarMejor f a b = if f a b then a else b

{-d) Definir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que
aparecen en el subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol
derecho-}
esABB :: Ord a => AB a -> Bool
esABB = recrAB (\esABBIzq r esABBDer i d -> esABBIzq && esABBDer && (esNil i || raiz i <= r) && (esNil d || raiz d > r)) True
    where raiz (Bin _ r _) = r

{-e) Justificar la elección de los esquemas de recursión utilizados para los tres puntos anteriores-}
{-
altura: Sólo necesitamos el dato de la recursión sobre las subestructuras -> recursión estructural
cantNodos: También, ya que nos basta saber la cantidad de nodos de los subárboles para obtener la del árbol entero, solo usamos recursión estructural
mejorSegún: Queremos comparar los mejores de los subárboles con la raíz, según el criterio dado. Por lo que nos basta con recursión estructural
esABB: Necesitamos acceder a las raíces de los subárboles sin la recursión para poder compararlas con la raíz del árbol entero y así determinar si se cumple la condición de ABB. 
    Entonces usamos recursión primitiva.
-}

{-Ejercicio 13-}
{-Dado el tipo AB a del ejercicio 12:
1) Definir las funciones ramas (caminos desde la raíz hasta las hojas), cantHojas y espejo.-}
ramas :: AB a -> [[a]]
ramas = recrAB (\ramasIzq r ramasDer i d -> f r i ramasIzq ++ f r d ramasDer) []
                where raiz Nil = []
                      raiz (Bin _ r _) = [r]

f :: a -> AB a -> [[a]] ->[[a]]
f r i xs |esNil i   = []
         |null xs   = [r:raiz i]
         |otherwise = (r:raiz i) : xs
         where raiz Nil = []
               raiz (Bin _ r _) = [r]
caminosDeRaizHastaHojas :: AB a -> [[a]]
caminosDeRaizHastaHojas = foldAB agregarACaminos []
                        where agregarACaminos xss r yss |null xss && null yss = [[r]]
                                                        |null xss             = map (r:) yss
                                                        |null yss             = map (r:) xss
                                                        |otherwise            = map (r:) (xss ++ yss)


espejo :: AB a -> AB a
espejo = foldAB (\recIzq r recDer -> Bin recDer r recIzq) Nil

--    2
--   / \
--  1   3
--  |\  |\
--  4 5 8 7
--         \
--         11
-- ramas de los subarboles de 2: [[1,4],[3,8],[1,5],[3,7]]
arb1 = Bin (Bin (Bin Nil 4 Nil) 1 (Bin Nil 5 Nil)) 2 (Bin (Bin Nil 8 Nil) 3 (Bin Nil 7 (Bin Nil 11 Nil)))
arb2 = Bin (Bin (Bin Nil 'c' Nil) 'a' (Bin Nil 'b' Nil)) 'd' (Bin (Bin Nil 'e' Nil) 'f' (Bin Nil 'g' (Bin Nil 'l' Nil)))

{-2) Definir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos árboles, indica si éstos
tienen la misma forma, independientemente del contenido de sus nodos. Pista: usar evaluación parcial y recordar el ejercicio 8-}
mismaEstructuraExplicita :: AB a -> AB b -> Bool
mismaEstructuraExplicita (Bin i r d)  (Bin l c ri) = mismaEstructuraExplicita i l && mismaEstructuraExplicita d ri
mismaEstructuraExplicita Nil Nil = True
mismaEstructuraExplicita Nil (Bin i r d) = False
mismaEstructuraExplicita (Bin i r d) Nil = False
--Le paso uno de los argumentos al otro lado
mismaEstructuraExplicita2 :: AB a -> AB b -> Bool
mismaEstructuraExplicita2 (Bin i r d) = \arbol -> if esNil arbol then False else mismaEstructuraExplicita2 i (hijoIzq arbol) && mismaEstructuraExplicita2 d (hijoDer arbol)
                                        where hijoDer (Bin _ _ d) = d
                                              hijoIzq (Bin i _ _) = i
mismaEstructuraExplicita2 Nil = \arbol -> esNil arbol


--Ahora reescribimos haciendo recursión sobre arbol Bin i r d
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB (\recIzq r recDer arbol -> if esNil arbol then False else recIzq (hijoIzq arbol) && recDer (hijoDer arbol)) esNil
                where hijoDer (Bin _ _ d) = d
                      hijoIzq (Bin i _ _) = i



{-
    2                    a
   / \                  / \
  1   3                b   c
  |\  |\              / | / \
  4 5 8 7            d  e f  g
         \                    \
         11                    h



-}

{-foldr :: (a -> b -> b) -> b -> [a] -> b
si la lambda es (\x rec ys -> ...) x::a , rec ys :: b por lo que rec :: b -> b y ys :: b -}

{-Ejercicio 14-}
{-Se desea modelar en Haskell los árboles con información en las hojas (y sólo en ellas). Para esto introduciremos
el siguiente tipo:-}
data AIH a = Hoja a | Binn (AIH a) (AIH a) --deriving Show
{-a) Definir el esquema de recursión estructural foldAIH y dar su tipo. Por tratarse del primer esquema de
recursión que tenemos para este tipo, se permite usar recursión explícita.
Hoja :: a -> AIH a
Bin :: AIH a -> AIH a -> AIH a
Reemplazamos los AIH a por b-}
foldAIH :: (b -> b -> b) -> (a -> b) -> AIH a -> b
foldAIH casoRec casoHoja arbol = case arbol of
                                Hoja k -> casoHoja k
                                Binn i d -> casoRec (rec i) (rec d)
                                where rec = foldAIH casoRec casoHoja
{-b) Escribir las funciones altura :: AIH a -> Integer y tamaño :: AIH a -> Integer.
Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas.-}

alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (\recI recD -> 1 + max recI recD) (const 1)

tamaño :: AIH a -> Integer
tamaño = foldAIH (+) (const 1)


arbAIH = Binn (Binn (Hoja 4) (Hoja 5)) (Binn (Hoja 8) (Binn (Hoja 7) (Hoja 11)))
{-  _____
   /\   /\
  4  5 8  |
         / \
        7  11
-}

{-Ejercicio 15-}
{-Definir el tipo RoseTree de árboles no vacíos, con una cantidad indeterminada de hijos para cada nodo.-}
data Rose a = Node a [Rose a]
-- Node :: a -> [Rose a] -> Rose a
{-2) Escribir el esquema de recursión estructural para RoseTree. Importante escribir primero su tipo.-}
foldRose :: (a -> [b] -> b) -> Rose a -> b
foldRose  f (Node a hijos) = f a (map (foldRose f) hijos) --Como tiene hijos en lista quiero hacer foldRose en cada uno por eso map
{- foldr :: (a -> b -> b) -> b -> [a] -> b
   foldr _ z []     = z
   foldr f z (x:xs) = f x (foldr f z xs)       vemos como aplica f al primer elemento y hace recursión consigo misma y baseCase sobre cola, lo mismo debe hacer foldRose
nombreFold funcion baseCase estructura pattern matching constructores = funcion primerElem (nombreFold funcion baseCase subestructura-}

{-3) Usando el esquema definido, escribir las siguientes funciones:
    a) hojas, que dado un RoseTree, devuelva una lista con sus hojas ordenadas de izquierda a derecha,
        según su aparición en el RoseTree.
    b) distancias, que dado un RoseTree, devuelva las distancias de su raíz a cada una de sus hojas.
    c) altura, que devuelve la altura de un RoseTree (la cantidad de nodos de la rama más larga). Si el
        RoseTree es una hoja, se considera que su altura es 1.-}
hojasRose :: Rose a -> [a]
hojasRose = foldRose (\nodo listaConLasListasDeRecSobreHijos -> nodo: concat listaConLasListasDeRecSobreHijos)

distancias :: Rose a -> [Int]
distancias = foldRose (\nodo listaConLasListasDeDistanciasDeSusHijos -> if null listaConLasListasDeDistanciasDeSusHijos then [0] else map (1+) (concat listaConLasListasDeDistanciasDeSusHijos))

alturaRose :: Rose a -> Int
alturaRose = foldRose (\nodo listaConListaDeAlturasDeSusHijos -> if null listaConListaDeAlturasDeSusHijos then 1 else 1 + maximum listaConListaDeAlturasDeSusHijos)
--OBS CLAVE: rec de la lambda en foldRose es del tipo [b] por lo tanto siempre es lista de tipo retorno, en base a eso veo si usa concat o no

rosetree = (Node 3 [Node 1 [Node 2 [], Node 4 [Node 5 []]], Node 7 [Node 23 [], Node 21 []]])

-- instance Show a => Show (Rose a) where
--     show = showRoseTree 0
--       where
--         showRoseTree :: Show a => Int -> Rose a -> String
--         showRoseTree indent (Node value children) =
--             replicate indent ' ' ++ show value ++ "\n" ++
--             concatMap (showRoseTree (indent + 2)) children

-- Árbol vacío
emptyTree :: AB a
emptyTree = Nil

-- Árbol con un solo nodo
singleNodeTree :: AB Int
singleNodeTree = Bin Nil 1 Nil

-- Árbol binario simple
simpleTree :: AB Int
simpleTree = Bin (Bin Nil 2 Nil) 1 (Bin Nil 3 Nil)

-- Árbol desequilibrado a la izquierda
leftHeavyTree :: AB Char
leftHeavyTree = Bin (Bin (Bin Nil 'a' Nil) 'b' Nil) 'c' Nil

-- Árbol desequilibrado a la derecha
rightHeavyTree :: AB String
rightHeavyTree = Bin Nil "root" (Bin Nil "right" (Bin Nil "rightmost" Nil))

-- Árbol completo de profundidad 3
completeTree :: AB Int
completeTree = Bin
                 (Bin (Bin Nil 1 Nil) 2 (Bin Nil 3 Nil))
                 4
                 (Bin (Bin Nil 5 Nil) 6 (Bin Nil 7 Nil))

-- Árbol con diferentes tipos de datos
mixedTypeTree :: AB (Either Int String)
mixedTypeTree = Bin
                  (Bin Nil (Left 1) Nil)
                  (Right "root")
                  (Bin Nil (Left 2) (Bin Nil (Right "leaf") Nil))
{-Ejercicio 16 (Opcional) -}
{- Se desea representar conjuntos mediante Hashing abierto (chain addressing). El Hashing abierto consta de
dos funciones: una función de Hash, que dado un elemento devuelve un valor entero (el cual se espera que no se
repita con frecuencia), y una tabla de Hash, que dado un número entero devuelve los elementos del conjunto a
los que la función de Hash asignó dicho número (es decir, la preimagen de la función de Hash para ese número).
Los representaremos en Haskell de la siguiente manera:-}
data HashSet a = Hash (a -> Integer) (Integer -> [a]) 

{-Por contexto de uso, vamos a suponer que la tabla de Hash es una función total, que devuelve listas vacías
para los números que no corresponden a elementos del conjunto. Este es un invariante que deberá preservarse
en todas las funciones que devuelvan conjuntos.
Definir las siguientes funciones:-}
{-1) vacío :: (a -> Integer) -> HashSet a, que devuelve un conjunto vacío con la función de Hash indicada.-}
vacío :: (a -> Integer) -> HashSet a
vacío f = Hash f (const [])

{-2) pertenece :: Eq a => a -> HashSet a -> Bool, que indica si un elemento pertenece a un conjunto. Es 
decir, si se encuentra en la lista obtenida en la tabla de Hash para el número correspondiente a la función
de Hash del elemento.
Por ejemplo:
pertenece 5 $ agregar 1 $ agregar 2 $ agregar 1 $ vacío (flip mod 5) devuelveFalse.
pertenece 2 $ agregar 1 $ agregar 2 $ agregar 1 $ vacío (flip mod 5) devuelveTrue.-}
pertenece :: Eq a => a -> HashSet a -> Bool
pertenece e (Hash funcionHash tablaHash) = e `elem` tablaHash (funcionHash e)
--funcionHash e genera la clave. tablaHash con la clave retorna lista de elementos del conjunto con esa clave


{-3) agregar :: Eq a => a -> HashSet a -> HashSet a, que agrega un elemento a un conjunto. Si el elemento ya estaba en el conjunto, se debe devolver el conjunto sin modificaciones.-}
agregar :: Eq a => a -> HashSet a -> HashSet a
agregar a (Hash funcionHash tablaHash) = if a `elem` tablaHash (funcionHash a) then Hash funcionHash tablaHash else Hash funcionHash (\x -> a:tablaHash(funcionHash a))
--pertenece 71(agregar 71(agregar 23(agregar 2(agregar 5 (vacío (flip mod 5))))))
hash = agregar 71(agregar 23(agregar 2(agregar 5 (vacío (flip mod 5)))))
hash2 = agregar 1(agregar 28(agregar 2(agregar 6 (vacío (*5)))))
{-4) intersección :: Eq a => HashSet a -> HashSet a -> HashSet a que, dados dos conjuntos, devuelve un conjunto con la misma función de Hash del primero y con los elementos que pertenecen a ambos
conjuntos a la vez.-}
-- intersección :: Eq a => HashSet a -> HashSet a -> HashSet a
-- intersección (Hash funcionHash1 tablaHash1) (Hash funcionHash2 tablaHash2) = Hash funcionHash1 (\clave -> if (clave `elem` tablaHash1 (funcionHash1 clave)) && (clave `elem` tablaHash2 (funcionHash2 clave)) then [clave] else []) 
--dados dos hashsets con la misma funcion de hash si n está en uno y en otro -> Está en intersección
--si a un hashset le cambio la funcion de hash pero dejo su tabla, 
--fhash dado un elemento -> indice en tabla de elementos con esa clave
--cuando agregué elementos antes de cambiarle la hash iban a otros indices de tabla. Ahora que la cambié no puedo saber si ese elemento consultado con la nueva esta en tabla.
---Que garantiza que si cambio funcion de hash pueda acceder a los mismo elementos?
--Se agrega elemento en pos que dice fhash

--t debe ser :: Integer -> [a] y retornar lista con los que estaban en tablaHash1 y 2
--t recibe un numero que es una clave, para ver si hay uno en tabla1 hacemos clave tabla1 

{-5) foldr1(no relacionada con los conjuntos). Dar el tipo y definir la función foldr1 para listas sin usar
recursión explícita, recurriendo a alguno de los esquemas de recursión conocidos.
Se recomienda usar la función error :: String -> a para el caso de la lista vacía.-}
foldr11 :: (a -> a -> a) -> [a] -> a --No tiene tipo b porque al usar como acumulador el último elemento de la lista y ser este del mismo tipo de la lista -> tipo retorno debe ser del mismo
foldr11 f = foldr f (error "No se puede utilizar foldr1 con lista vacía")--Usamos como acumulador último elemento lista


{-Generación infinita-}
{-Ejercicio 17-}
-- ¿Cuál es el valor de esta expresión?
lista1a3 = [ x | x <- [1..3], y <- [x..3], (x + y) `mod` 3 == 0 ]
{-RTA: Retona una lista con todos los x de 1 a 3 que sumados a otro numero de x a 3 den 3.
x = 1 y = 1, 2 mod 3 != 0 -> no agrego 1, x = 1 y = 2, 3 mod 3 == 0 -> agrego 1, x = 1 y = 3 , 4 mod 3 != 0 -> no agrego 1 .:. 1 pertenece a lista
x = 2 y = 2, 4 mod 3 != 0 -> no agrego 2, x = 2 y = 3, 5 mod 3 != 0 -> no agrego 2 .:. 2 no pertenece
x = 3 y = 3, 6 mod 3 == 0 -> agrego 3 .:. 3 pertenece
Luego la lista es [1,3]-}

{-Ejercicio 18-}
--Definir la lista infinita paresDeNat::[(Int,Int)], que contenga todos los pares de números naturales: (0,0), (0,1), (1,0), etc.
paresDeNat :: [(Int,Int)]
--paresDeNat = [(x,y) | x <- [1..], y <- [1..x]] --Genera solo los que crecen hacia un lado, e.g. (3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4),etc...
paresDeNat = [(x,y) | s <- [1..], x <- [1..s], y <- [1..s], x + y == s]

{-Ejercicio 19-}
--Una tripla pitagórica es una tripla (a, b, c) de enteros positivos tal que a2+ b2= c2.
--La siguiente expresión intenta ser una definición de una lista (infinita) de triplas pitagóricas:
pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]
--Explicar por qué esta definición no es útil. Dar una definición mejor.
{-RTA: No es útil porque como hay varios generadores infinitos, el c nunca va a parar y no va a permitir que el b y a avancen, entonces se quedan clavados en 1-}
pitagoricasBien :: [(Integer, Integer, Integer)]
pitagoricasBien = [(a, b, c) | s <- [1..], a <- [1..s], b <- [1..s-a], c <- [1..s-a-b], a^2 + b^2 == c^2]

testPitagoricas :: [(Integer, Integer, Integer)] -> Bool
testPitagoricas = foldr (\x rec -> ((fst x)^2 + (snd x)^2 == (thrd x)^2) && rec) True
                    where thrd (_,_,a) = a
                          snd (_,a,_)  = a
                          fst (a,_,_)  = a
{-Ejercicio 20-}
{-Escribir la función listasQueSuman :: Int -> [[Int]] que, dado un número natural n, devuelve todas las
listas de enteros positivos (es decir, mayores o iguales que 1) cuya suma sea n. Para este ejercicio se permite
usar recursión explícita. Pensar por qué la recursón utilizada no es estructural. (Este ejercicio no es de
generación infinita, pero puede ser útil para otras funciones que generen listas infinitas de listas).-}
listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = []
listasQueSuman n = [xs | xs <- listasQueSumanNDeLongitd n n]

listasQueSumanNDeLongitd :: Int -> Int -> [[Int]]
listasQueSumanNDeLongitd 0 0 = [[]]
listasQueSumanNDeLongitd l n = [x:xs | x <- [1..n], xs <- listasQueSumanNDeLongitd (l-1) (n-x)] 

testListasQueSumanN :: Int -> [[Int]] -> Bool
testListasQueSumanN n = foldr (\x rec -> sum x == n && rec) True
--Por qué no es estructural? 

{-Ejercicio 21-}
--Definir en Haskell una lista que contenga todas las listas finitas de enteros positivos (esto es, con elementos mayores o iguales que 1).
listasDeEnteros :: [[Int]]
listasDeEnteros = [x:xs | s <- [1..], x <- [1..s], xs <- listasQueSuman (s-x)]

{-Ejercicio 22-}
--Dado el tipo de datos AIH a definido en el ejercicio 14:
--a) Definir la lista (infinita) de todos los AIH cuyas hojas tienen tipo (). Se recomienda definir una función auxiliar. Para este ejercicio se permite utilizar recursión explícita.
--b) Explicar por qué la recursión utilizada en el punto a) no es estructural.
--El tipo (), usualmente conocido como unit, tiene un único valor, denotado como ().
listaInfAIH :: [AIH ()]
listaInfAIH = []

listaAIHConNnodos :: AIH () -> [AIH ()]
listaAIHConNnodos 1  Hoja _ = []
listaAIHConNnodos n ()= 