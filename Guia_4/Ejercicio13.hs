-- Ejercicio 13. Especificar e implementar la siguiente funcion:
--          n   m
-- f(n,m) = ∑   ∑ i^j , n, m ∈ N0
--         i=1 j=1

f :: Integer -> Integer -> Integer
f 0 m = 0
f n m = suma_interna n m + f (n - 1) m

suma_interna :: Integer -> Integer -> Integer
suma_interna i 0 = 0
suma_interna i j = i^j + suma_interna i (j - 1)