-- Ejercicio 11. a) Especificar e implementar una funcion eAprox :: Integer ->Float que aproxime el valor del numero e
-- a partir de la siguiente sumatoria:

--         n
-- e^(n) = ∑ 1/i! , n ∈ N0
--        i=0

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial(n-1)

e_aprox :: Integer ->  Float
e_aprox 0 = 1
e_aprox n = 1/fromIntegral(factorial n) + e_aprox(n-1)