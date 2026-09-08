-- Ejercicio 9. Especificar e implementar una funcion esCapicua :: Integer ->Bool que dado n ∈ N ≥ 0 determina si n es
-- un numero capicua.

-- helpers
cant_digitos :: Integer -> Integer
cant_digitos 0 = 1
cant_digitos n  |   n < 10 = 1
                |   otherwise = 1 + cant_digitos(div n 10)

iesimo_digito :: Integer -> Integer -> Integer
iesimo_digito n i = mod (div n (10^(cant_digitos n-i))) 10


-- implementacion
es_capicua :: Integer -> Bool
es_capicua n    |   n < 10 = True
                |   iesimo_digito n (cant_digitos n) == iesimo_digito n 1 = es_capicua (mod (div n 10) (10^(cant_digitos(n) -2)))
                |   otherwise = False