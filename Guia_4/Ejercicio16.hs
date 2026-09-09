-- Ejercicio 16. Recordemos que un entero p > 1 es primo si y solo si no existe un entero k tal que 1 < k < p y k divida a p.

-- a) Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado
-- como parametro.

-- b) Implementar la funcion esPrimo :: Integer ->Bool que indica si un numero natural pasado como parametro es primo.

-- c) Implementar la funcion sonCoprimos :: Integer ->Integer ->Bool que dados dos numeros naturales indica si no
-- tienen algun divisor en comun mayor estricto que 1.

-- d) Implementar la funcion nEsimoPrimo :: Integer ->Integer que devuelve el n-esimo primo (n ≥ 1). Recordar que el
-- primer primo es el 2, el segundo es el 3, el tercero es el 5, etc

buscar_menor_divisor :: Integer -> Integer -> Integer
buscar_menor_divisor n m    |   mod n m == 0 = m
                            |   otherwise = buscar_menor_divisor n (m+1)

buscar_maximo_divisor :: Integer -> Integer -> Integer
buscar_maximo_divisor n m   |   mod n m == 0 = m
                            |   otherwise = buscar_maximo_divisor n (m-1)

menor_divisor :: Integer -> Integer
menor_divisor n = buscar_menor_divisor n 2

maximo_divisor :: Integer -> Integer
maximo_divisor n = buscar_maximo_divisor n (n-1)

es_primo :: Integer -> Bool
es_primo 1 = False
es_primo n = n == menor_divisor n

maximo_comun_divisor :: Integer -> Integer -> Integer -> Integer 
maximo_comun_divisor n m p  |   mod n p == 0 && mod m p == 0 = p
                            |   otherwise = maximo_comun_divisor n m (p-1)

son_coprimos :: Integer -> Integer -> Bool
son_coprimos 1 1 = True
son_coprimos n m    |   n > m && (maximo_comun_divisor n m m) == 1 = True
                    |   m > n && (maximo_comun_divisor m n n) == 1 = True
                    |   otherwise = False

buscar_n_esimo_primo :: Integer -> Integer -> Integer
buscar_n_esimo_primo 1 i
    | es_primo i = i
    | otherwise = buscar_n_esimo_primo 1 (i + 1)
buscar_n_esimo_primo n i
    | es_primo i = buscar_n_esimo_primo (n - 1) (i + 1)
    | otherwise = buscar_n_esimo_primo n (i + 1)


n_esimo_primo :: Integer -> Integer
n_esimo_primo n = buscar_n_esimo_primo n 2