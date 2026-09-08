-- Ejercicio 10. Especificar, implementar y dar el tipo de las siguientes funciones (sımil Ejercicio 4 Practica 2 de Algebra 1).
--           n
-- a) f(n) = ∑ 2^i , n ∈ N0.
--          i=0

--             n
-- b) f(n,q) = ∑ q^i , n ∈ N y q ∈ R
--            i=1

--            2n
-- c) f(n,q) = ∑ q^i , n ∈ N0 y q ∈ R
--            i=1

--            2n
-- d) f(n,q) = ∑ q^i , n ∈ N0 y q ∈ R
--            i=n

f1 :: Integer -> Integer
f1 n    |   n == 0 = 1
        |   otherwise = 2^n + f1(n-1)

f2 :: Integer -> Float -> Float
f2 n q  |   n == 0 = 1
        |   n == 1 = q
        |   otherwise = q^n + f2(n-1) q

f3 :: Integer -> Float -> Float
f3 n q = f2 (n*2) q

f4 :: Integer -> Float -> Float
f4 n q = f4_limite_actual (n*2) n q

f4_limite_actual :: Integer -> Integer -> Float -> Float
f4_limite_actual n m q  |   n == m = q ^ n
                        |   otherwise = q ^ m + f4_limite_actual n (m+1) q