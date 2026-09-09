-- Ejercicio 18. Implementar una funcion mayorDigitoPar :: Integer ->Integer segun la siguiente especificacion:
-- problema mayorDigitoPar (n: N) : N {
-- requiere: { True }
-- asegura: { resultado es el mayor de los dıgitos pares de n. Si n no tiene ningun dıgito par, entonces resultado es -1.}
-- }

guardar_mayor_digito_par_actual :: Integer -> Integer -> Integer
guardar_mayor_digito_par_actual n m
    | n < 10 && (mod n 2) == 0 && (n > m) = n
    | n < 10 = m
    | mod (mod n 10) 2 == 0 && mod n 10 > m = guardar_mayor_digito_par_actual (div n 10) (mod n 10)
    | otherwise = guardar_mayor_digito_par_actual (div n 10) m

mayor_digito_par :: Integer -> Integer
mayor_digito_par n = guardar_mayor_digito_par_actual n (-1)