-- Ejercicio 20. Especificar e implementar la funcion tomaValorMax :: Integer ->Integer ->Integer que dado un numero
-- entero n ≥ 1 y un m ≥ n devuelve algun k entre n y m tal que sumaDivisores(k) = max{sumaDivisores(i) | n1 ≤ i ≤ n2}

suma_interna_divisores :: Integer -> Integer -> Integer
suma_interna_divisores n 1 = 1
suma_interna_divisores n m
    |   (mod n m) == 0 = (m + suma_interna_divisores n (m-1))
    |   otherwise = suma_interna_divisores n (m-1)


suma_divisores :: Integer -> Integer
suma_divisores n = suma_interna_divisores n n

guardado_maximo_suma_divisores :: Integer -> Integer-> Integer -> Integer
guardado_maximo_suma_divisores maximo_actual i limite_inferior
    |   i == limite_inferior && (suma_divisores maximo_actual) >= (suma_divisores i) = maximo_actual
    |   i == limite_inferior = i
    |   (suma_divisores maximo_actual) >= (suma_divisores i) = guardado_maximo_suma_divisores maximo_actual (i-1) limite_inferior
    |   otherwise = guardado_maximo_suma_divisores i (i-1) limite_inferior

toma_valor_max :: Integer -> Integer -> Integer
toma_valor_max n m = guardado_maximo_suma_divisores n m n