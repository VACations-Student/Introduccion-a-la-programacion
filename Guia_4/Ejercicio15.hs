-- Ejercicio 15. Implementar una funcion sumaRacionales :: Integer ->Integer ->Float que dados dos naturales n, m
-- sume todos los numeros racionales de la forma p/q con 1 ≤ p ≤ n y 1 ≤ q ≤ m, es decir:
-- problema sumaRacionales (n : N, m : N) : R {
-- requiere: { T rue}
-- asegura: { resultado =
--          n   m
-- f(n,m) = ∑   ∑ p/q , n, m ∈ N
--         p=1 q=1

f :: Integer -> Integer -> Float
f 0 m = 0
f n m = suma_interna n m + f (n - 1) m

suma_interna :: Integer -> Integer -> Float
suma_interna i 0 = 0
suma_interna i j = fromIntegral i/fromIntegral j + suma_interna i (j - 1) 