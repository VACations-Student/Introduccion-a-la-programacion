-- Ejercicio 2. ⋆ Especificar e implementar las siguientes funciones, incluyendo su signatura.
-- a) absoluto: calcula el valor absoluto de un numero entero

-- a)
-- Especificacion:
-- calcular_absoluto (n : Z) : Z {
-- requiere: { TRUE }
-- asegura:  { (n >= 0 → res = n) ∨ (n < 0 → res = -n) }
-- }

calcular_absoluto :: Integer -> Integer 
calcular_absoluto n
                | n >= 0 = n
                | otherwise = -n

-- b) maximoAbsoluto: devuelve el maximo entre el valor absoluto de dos numeros enteros.
-- Especificacion:
-- maximo_absoluto (n : Z, m : Z) : Z {
-- requiere: { TRUE }
-- asegura:  { (|n| > |m| → res = |n|) ∨ (|m| > |n| → res = |m|) ∨ (|n| == |m| → res = |n|) }
-- }

maximo_absoluto :: Integer -> Integer -> Integer
maximo_absoluto n m
                | calcular_absoluto(n) >= calcular_absoluto(m) = calcular_absoluto(n)
                | otherwise = calcular_absoluto m

-- c) maximo3: devuelve el maximo entre tres numeros enteros
-- Especificacion:
-- maximo_3 (n : Z, m : Z, k : Z) : Z {
-- requiere: { TRUE }
-- asegura:  { (n > m ∧ n > k → res = n) ∨ (m > n ∧ m > k → res = m) ∨ (k > n ∧ k > m → res = k) ∨ (n == m ∧ n == k → res = n) }
-- }

maximo_2 :: Integer -> Integer -> Integer
maximo_2 n m
            | n >= m = n
            | otherwise = m

maximo_3 :: Integer -> Integer -> Integer -> Integer
maximo_3 n m k
            | maximo_2 n m == maximo_2 n k = n
            | otherwise = maximo_2 m k

-- d) algunoEsCero: dados dos numeros racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching).
-- Especificacion:
-- alguno_es_cero (x : Q, y : Q) : Bool {
-- requiere: { TRUE }
-- asegura:  { (x == 0 → res = True) ∨ (y == 0 → res = True) ∨ (x != 0 ∧ y != 0 → res = False)}
-- }

alguno_es_cero_pattern_matching :: Float -> Float -> Bool
alguno_es_cero_pattern_matching x  y | x == 0 = True
                                     | y == 0 = True
                                     | otherwise = False

alguno_es_cero_sin_pattern_matching :: Float -> Float -> Bool
alguno_es_cero_sin_pattern_matching x y = x == 0 || y == 0

-- e) ambosSonCero: dados dos n´umeros racionales, decide si ambos son iguales a 0 (resolverlo con y sin pattern matching).
-- Especificacion:
-- ambos_son_cero (x : Q, y : Q) : Bool {
-- requiere: { TRUE }
-- asegura:  { (x != 0 → res = False) ∨ (y != 0 → res = False) ∨ (x == 0 ∧ y == 0 → res = True)}
-- }

ambos_son_cero_pattern_matching :: Float -> Float -> Bool
ambos_son_cero_pattern_matching x  y | x == 0 = False
                                     | y == 0 = False
                                     | otherwise = True

ambos_son_cero_sin_pattern_matching :: Float -> Float -> Bool
ambos_son_cero_sin_pattern_matching x y = x == 0 && y == 0

-- f) enMismoIntervalo: dados dos numeros reales, indica si estan relacionados por la relacion de equivalencia en R cuyas
-- clases de equivalencia son: (−∞, 3], (3, 7] y (7, ∞), o dicho de otra manera, si pertenecen al mismo intervalo.
-- Especificacion:
-- en_mismo_intervalo (x: R, y : R) : Bool {
-- requiere: { ((x ∈ (-∞, 3]) ∨ (x ∈ (3, 7]) ∨ (x ∈ (7, ∞))) ∧ ((y ∈ (-∞, 3]) ∨ (y ∈ (3, 7]) ∨ (y ∈ (7, ∞))) } || creo que en realidad solo requiere TRUE jeje
-- asegura:  { (x ∈ (−∞, 3] ∧ y ∈ (−∞, 3] → res = True) ∨ (x ∈ (3, 7] ∧ y ∈ (3, 7] → res = True) ∨ (x ∈ (7, ∞) ∧ y ∈ (7, ∞) → res = True) }
-- }

en_mismo_intervalo :: Double -> Double -> Bool
en_mismo_intervalo x y  | x <= 3 && y <= 3 = True
                        | x > 3 && x <= 7 && y > 3 && y <= 7 = True
                        | x > 7 && y > 7 = True
                        | otherwise = False

-- g) sumaDistintos: que dados tres numeros enteros calcule la suma sin sumar repetidos (si los hubiera).
-- Especificacion:
-- suma_distintos (n : Z, m : Z, k : Z) : Z {
-- requiere: { TRUE }
-- asegura:  { (n != m ∧ n != k ∧ m != k → res = n + m + k) ∨ (n != m ∧ m == k → res = n + m) 
-- ∨ (n == m ∧ m != k → res = n + k) ∨ (n == k ∧ n != m → res = n + m) ∨ (n == m ∧ m == k → res = 0) }
-- }

suma_distintos :: Integer -> Integer -> Integer -> Integer
suma_distintos n m k    | n /= m && n /= k && m /= k = n + m + k
                        | n == m && m == k = 0
                        | n == m = n + k
                        | n == k = n + m
                        | m == k = n + m

-- h) esMultiploDe: dados dos numeros naturales, decide si el primero es multiplo del segundo.
-- Especificacion:
-- es_multiplo_de (n : N, m : N) : N {
-- requiere: { (n > 0) ∧ (m > 0) } 
-- asegura:  { (n == m * k → res = True) ∨ (n != m * k → res = False) }
-- }

es_multiplo_de :: Integer -> Integer -> Bool
es_multiplo_de n m = mod n m == 0

-- i) digitoUnidades: dado un numero entero, extrae su dıgito de las unidades.
-- Especificacion:
-- digito_unidades (n : Z) : Z {
-- requiere: { TRUE }
-- asegura:  { (n >= 10 → res = mod n 10) }
-- }

digito_unidades :: Integer -> Integer
digito_unidades n = mod n 10

-- j) digitoDecenas: dado un numero entero mayor a 9, extrae su dıgito de las decenas.
-- Especificacion:
-- digito_decenas (n : Z) : Z {
-- requiere: { TRUE }
-- asegura:  { n > 9 → res = (n / 10) mod 10 }
-- }

digito_decenas :: Integer -> Integer
digito_decenas n = mod (div n 10) 10