-- Ejercicio 14. Especificar e implementar una funcion sumaPotencias :: Integer ->Integer ->Integer ->Integer que
-- dados tres naturales q, n, m sume todas las potencias de la forma q^(a+b) con 1 ≤ a ≤ n y 1 ≤ b ≤ m

f :: Integer -> Integer -> Integer -> Integer
f 0 m q = 0
f n m q = suma_interna n m q + f (n - 1) m q

suma_interna :: Integer -> Integer -> Integer -> Integer
suma_interna i 0 q = 0
suma_interna i j q = q^(i+j) + suma_interna i (j - 1) q