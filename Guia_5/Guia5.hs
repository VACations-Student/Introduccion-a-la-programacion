-- Ejercicio 1. Definir las siguientes funciones sobre listas:
-- 1. longitud :: [t] -> Integer, que dada una lista devuelve su cantidad de elementos.
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs)
    |   otherwise = 1 + longitud xs

-- 2. ultimo :: [t] -> t segun la siguiente especificacion:
-- problema ultimo (s: seq⟨T ⟩) : T {
-- requiere: { |s| > 0 }
-- asegura: { resultado = s[|s| − 1] }
-- }

ultimo :: [t] -> t
ultimo (x:xs)
    |   longitud (x:xs) == 1 = x
    |   otherwise = ultimo xs

    -- 3. principio :: [t] -> [t] segun la siguiente especificacion:
-- problema principio (s: seq⟨T ⟩) : seq⟨T ⟩ {
-- requiere: { |s| > 0 }
-- asegura: { resultado = subseq(s, 0, |s| − 1) }
-- }

principio :: [t] -> [t]
principio (x:xs)
    |   longitud (x:xs) == 1 = []
    |   otherwise = x : principio xs

-- 4. reverso :: [t] -> [t] segun la siguiente especificacion:
-- problema reverso (s: seq⟨T ⟩) : seq⟨T ⟩ {
-- requiere: { True }
-- asegura: { resultado tiene los mismos elementos que s pero en orden inverso.}
-- }

reverso :: [t] -> [t]
reverso (x:xs)
    |   longitud (x:xs) == 1 = x : []
    |   otherwise = ultimo (x:xs) : reverso (principio (x:xs))


-- Ejercicio 2. Definir las siguientes funciones sobre listas:

-- 1. pertenece :: (Eq t) => t -> [t] -> Bool segun la siguiente especificacion:
-- problema pertenece (e: T , s: seq⟨T ⟩) : B {
-- requiere: { True }
-- asegura: { resultado = true ↔ e ∈ s }
-- }

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs)
    |   x == n = True
    |   otherwise = pertenece n xs

-- todosIguales :: (Eq t) => [t] -> Bool, que dada una lista devuelve verdadero sı y solamente sı todos sus elementos son iguales.

todos_iguales :: (Eq t) => [t] -> Bool
todos_iguales [x] = True
todos_iguales (x:xs)
    |   pertenece x xs == True = todos_iguales xs
    |   otherwise = False


-- 3. todosDistintos :: (Eq t) => [t] -> Bool segun la siguiente especificacion:
-- problema todosDistintos (s: seq⟨T ⟩) : B {
-- requiere: { True }
-- asegura: { resultado = false ↔ existen dos posiciones distintas de s con igual valor }

todos_distintos :: (Eq t) => [t] -> Bool
todos_distintos [x] = True
todos_distintos (x:xs)
    |   pertenece x xs == False = todos_distintos xs
    |   otherwise = False

-- 4. hayRepetidos :: (Eq t) => [t] -> Bool segun la siguiente especificacion:
-- problema hayRepetidos (s: seq⟨T ⟩) : B {
-- requiere: { True }
-- asegura: { resultado = true ↔ existen dos posiciones distintas de s con igual valor }
-- }

hay_repetidos :: (Eq t) => [t] -> Bool
hay_repetidos x = not (todos_distintos x)

-- 5. quitar :: (Eq t) => t -> [t] -> [t], que dados un elemento e y una lista s, elimina la primera aparicion de e en la lista s (de haberla).
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar e (x:xs)
    |   e /= x = x : (quitar e xs)
    |   otherwise = xs

-- 6. quitarTodos :: (Eq t ) => t -> [t] -> [t], que dados un elemento e y una lista s, elimina todas las apariciones
-- de e en la lista s (de haberlas). Es decir:
-- problema quitarTodos (e: T , s: seq⟨T ⟩) : seq⟨T ⟩ {
-- requiere: { True }
-- asegura: { resultado es igual a s pero sin el elemento e. }
-- }

quitar_todos :: (Eq t ) => t -> [t] -> [t]
quitar_todos _ [] = []
quitar_todos e (x:xs)
    |   e /= x = x : (quitar_todos e xs)
    |   otherwise = quitar_todos e xs


-- 7. eliminarRepetidos :: (Eq t) => [t] -> [t] que deja en la lista una unica aparicion de cada elemento, eliminando las repeticiones adicionales.

eliminar_repetidos :: (Eq t) => [t] -> [t]
eliminar_repetidos [] = []
eliminar_repetidos (x:xs)
    |   not (hay_repetidos (x:xs)) = (x:xs)
    |   otherwise = x : (eliminar_repetidos (quitar_todos x xs))

-- 8. mismosElementos :: (Eq t) => [t] -> [t] -> Bool, que dadas dos listas devuelve verdadero sı y solamente sı
-- ambas listas contienen los mismos elementos, sin tener en cuenta repeticiones, es decir:
-- problema mismosElementos (s: seq⟨T ⟩, r: seq⟨T ⟩) : B {
-- requiere: { True }
-- asegura: { resultado = true ↔ todo elemento des pertenece r y viceversa}
-- }

mismos_elementos :: (Eq t) => [t] -> [t] -> Bool
mismos_elementos [] (y:ys) = True
mismos_elementos (x:xs) (y:ys)
    |   (pertenece x (y:ys)) = mismos_elementos xs (y:ys)
    |   otherwise = False

mismos_elementos' :: (Eq t) => [t] -> [t] -> Bool
mismos_elementos' (x:xs) (y:ys)
    |   eliminar_repetidos (x:xs) == eliminar_repetidos (y:ys) = True
    |   otherwise = False

-- 9. capicua :: (Eq t) => [t] -> Bool segun la siguiente especificacion:
-- problema capicua (s: seq⟨T ⟩) : B {
-- requiere: { True }
-- asegura: { (resultado = true) ↔ (s = reverso(s)) }
-- }
-- Por ejemplo capicua [´a’,’c’, ’b’, ’b’, ’c’, ´a’] es true, capicua [´a’, ’c’, ’b’, ’d’, ´a’] es false.

capicua :: (Eq t) => [t] -> Bool
capicua (x:xs) = (x:xs) == reverso (x:xs)

-- Ejercicio 3. Definir las siguientes funciones sobre listas de enteros:

-- 1. sumatoria :: [Integer] -> Integer segun la siguiente especificacion:
-- problema sumatoria (s: seq⟨Z⟩) : Z {
-- requiere: { True }
-- asegura:{       |s|-1         }
--         {  f(s) = ∑  s[i]     }
--         {        i=0          }
-- }

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- 2. productoria :: [Integer] -> Integer seg´un la siguiente especificaci´on:
-- problema productoria (s: seq⟨Z⟩) : Z {
-- requiere: { T rue }
-- asegura:{       |s|-1         }
--         {  f(s) = π  s[i]     }
--         {        i=0          }
-- }

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- 3. maximo :: [Integer] -> Integer segun la siguiente especificacion:
-- problema maximo (s: seq⟨Z⟩) : Z {
-- requiere: { |s| > 0 }
-- asegura: { resultado ∈ s ∧ todo elemento de s es menor o igual a resultado }
-- }

maximo :: [Integer] -> Integer
maximo [] = 0
maximo [x] = x
maximo (x:xs)
    |   x >= head xs = maximo (x:(tail xs))
    |   otherwise = maximo xs

-- 4. sumarN :: Integer -> [Integer] -> [Integer] segun la siguiente especificacion:
-- problema sumarN (n: Z, s: seq⟨Z⟩) : seq⟨Z⟩ {
-- requiere: { T rue }
-- asegura: {|resultado| = |s| ∧ cada posicion de resultado contiene el valor que hay en esa posicion en s sumado n}
-- }

sumar_n :: Integer -> [Integer] -> [Integer]
sumar_n _ [] = []
sumar_n n (x:xs) = (x + n) : sumar_n n xs

-- 5. sumarElPrimero :: [Integer] -> [Integer] segun la siguiente especificacion:
-- problema sumarElPrimero (s: seq⟨Z⟩) : seq⟨Z⟩ {
-- requiere: { |s| > 0 }
-- asegura: {resultado = sumarN (s[0], s) }
-- }

sumar_el_primero :: [Integer] -> [Integer]
sumar_el_primero (x:xs) = sumar_n x (x:xs)

-- 6. sumarElUltimo :: [Integer] -> [Integer] segun la siguiente especificacion:
-- problema sumarElUltimo (s: seq⟨Z⟩) : seq⟨Z⟩ {
-- requiere: { |s| > 0 }
-- asegura: {resultado = sumarN (s[|s| − 1], s) }
-- }

sumar_el_ultimo :: [Integer] -> [Integer]
sumar_el_ultimo (x:xs) = sumar_n (ultimo (x:xs)) (x:xs)

-- 7. pares :: [Integer] -> [Integer] segun la siguiente especificacion:
-- problema pares (s: seq⟨Z⟩) : seq⟨Z⟩ {
-- requiere: { True }
-- asegura: {resultado solo tiene los elementos pares de s en el orden dado, respetando las repeticiones}
-- }

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
    |   (mod x 2) == 0 = x : pares xs
    |   otherwise = pares xs

-- 8. multiplosDeN :: Integer -> [Integer] -> [Integer] que dado un numero n y una lista xs, devuelve una lista
-- con los elementos de xs multiplos de n.

multiplos_de_n :: Integer -> [Integer] -> [Integer]
multiplos_de_n _ [] = []
multiplos_de_n n (x:xs)
    |   (mod x n) == 0 = x : multiplos_de_n n xs
    |   otherwise = multiplos_de_n n xs

-- 9. ordenar :: [Integer] -> [Integer] que ordena los elementos de la lista en forma creciente. Sugerencia: Pensar
-- como pueden usar la funcion maximo para que ayude a generar la lista ordenada necesaria.

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar x = ordenar (quitar (maximo x) x) ++ [maximo x]

ordenar_decreciente :: [Integer] -> [Integer]
ordenar_decreciente [] = []
ordenar_decreciente x = maximo x: ordenar_decreciente (quitar (maximo x) x)

-- Ejercicio 4. a) Definir las siguientes funciones sobre listas de caracteres, interpretando una palabra como una secuencia de
-- caracteres sin blancos:

-- a) sacarBlancosRepetidos :: [Char] -> [Char], que reemplaza cada subsecuencia de blancos contiguos de la pri-
-- mera lista por un solo blanco en la lista resultado.

sacar_blancos_repetidos :: [Char] -> [Char]
sacar_blancos_repetidos [] = []
sacar_blancos_repetidos [x] = [x]
sacar_blancos_repetidos (x:xs)
    |   x == ' ' && head xs == ' ' = x : sacar_blancos_repetidos (tail xs)
    |   otherwise = (x : sacar_blancos_repetidos xs)

-- b) contarPalabras :: [Char] -> Integer, que dada una lista de caracteres devuelve la cantidad de palabras que
-- tiene.

contar_palabras :: [Char] -> Integer
contar_palabras [] = 0
contar_palabras [x] = 1
contar_palabras (x:xs)
    |   x /= ' ' && head xs == ' ' = 1 + contar_palabras (tail xs)
    |   otherwise = (0 + contar_palabras xs)
 
-- c) palabras :: [Char] -> [[Char]], que dada una lista arma una nueva lista con las palabras de la lista original.



-- d ) palabraMasLarga :: [Char] -> [Char], que dada una lista de caracteres devuelve su palabra mas larga.

-- e) aplanar :: [[Char]] -> [Char], que a partir de una lista de palabras arma una lista de caracteres concaten´ando-
-- las.

-- f ) aplanarConBlancos :: [[Char]] -> [Char], que a partir de una lista de palabras, arma una lista de caracteres
-- concatenandolas e insertando un blanco entre cada palabra.

-- g) aplanarConNBlancos :: [[Char]] -> Integer -> [Char], que a partir de una lista de palabras y un entero n,
-- arma una lista de caracteres concaten´andolas e insertando n blancos entre cada palabra (n debe ser no negativo).