-- Ejercicio 17. Implementar la funcion esFibonacci :: Integer ->Bool segun la siguiente especificacion:
-- problema esFibonacci (n: Z) : B {
-- requiere: { n ≥ 0 }
-- asegura: { resultado = true ↔ n es algun valor de la secuencia de Fibonacci definida en el ejercicio 1}
-- }

fibonacci :: Integer -> Integer
fibonacci   n   | n == 0 = 0
                | n == 1 = 1
                | otherwise = fibonacci(n - 1) + fibonacci(n - 2)

recorrer_fibonacci_hasta_econtrar_n :: Integer -> Integer -> Bool
recorrer_fibonacci_hasta_econtrar_n n m 
    | n == fibonacci m = True
    | fibonacci m < n = recorrer_fibonacci_hasta_econtrar_n n (m + 1)
    | otherwise = False

es_fibonacci :: Integer -> Bool
es_fibonacci n = recorrer_fibonacci_hasta_econtrar_n n 0