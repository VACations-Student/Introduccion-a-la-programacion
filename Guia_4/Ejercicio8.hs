-- Ejercicio 8. Especificar e implementar la funcion sumaDigitos :: Integer ->Integer que calcula la suma de dıgitos de
-- un numero natural. Para esta funcion pueden utilizar div y mod.

suma_digitos :: Integer -> Integer
suma_digitos n  |   n < 10 = n
                |   otherwise = mod n 10 + suma_digitos(div n 10)