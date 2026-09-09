-- Ejercicio 19. Implementar la funcion esSumaInicialDePrimos :: Integer ->Bool segun la siguiente especificacion:
-- problema esSumaInicialDePrimos (n: Z) : B {
-- requiere: { n ≥ 0 }
-- asegura: { resultado = true ↔ n es igual a la suma de los m primeros numeros primos, para algun m.}
-- }

buscar_menor_divisor :: Integer -> Integer -> Integer
buscar_menor_divisor n m    |   mod n m == 0 = m
                            |   otherwise = buscar_menor_divisor n (m+1)

menor_divisor :: Integer -> Integer
menor_divisor n = buscar_menor_divisor n 2

es_primo :: Integer -> Bool
es_primo 1 = False
es_primo n = n == menor_divisor n

buscar_n_esimo_primo :: Integer -> Integer -> Integer
buscar_n_esimo_primo 1 i
    | es_primo i = i
    | otherwise = buscar_n_esimo_primo 1 (i + 1)
buscar_n_esimo_primo n i
    | es_primo i = buscar_n_esimo_primo (n - 1) (i + 1)
    | otherwise = buscar_n_esimo_primo n (i + 1)


n_esimo_primo :: Integer -> Integer
n_esimo_primo n = buscar_n_esimo_primo n 2

sumar_n_e_i_esimo_primo :: Integer -> Integer -> Integer
sumar_n_e_i_esimo_primo n i = (n + n_esimo_primo i)

encontrar_numero_sumando_primos :: Integer -> Integer -> Integer -> Integer
encontrar_numero_sumando_primos n m i
    |   n == m = n
    |   m > n =  m
    |   otherwise = encontrar_numero_sumando_primos n (sumar_n_e_i_esimo_primo m i) (i+1)

es_suma_inicial_de_primos :: Integer -> Bool
es_suma_inicial_de_primos n
    |   n == (encontrar_numero_sumando_primos n 0 1) = True
    |   otherwise = False