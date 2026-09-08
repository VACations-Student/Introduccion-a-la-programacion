-- Ejercicio 3. Especificar e implementar la funcion esDivisible :: Integer ->Integer ->Bool que dados dos numeros
-- naturales determinar si el primero es divisible por el segundo. No esta permitido utilizar las funciones mod ni div.

es_divisible :: Integer -> Integer -> Bool
es_divisible a 1 = True
es_divisible 0 b = True
es_divisible 1 b = False
es_divisible a b
    |   a > b = es_divisible (a-b) b
    |   a < b = False
    |   otherwise = True