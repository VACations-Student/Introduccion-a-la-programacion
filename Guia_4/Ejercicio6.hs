-- Ejercicio 6. Implementar la funcion todosDigitosIguales :: Integer ->Bool que determina si todos los dıgitos de un
-- numero natural son iguales, es decir:
-- problema todosDigitosIguales (n: Z) : B {
-- requiere: { n > 0 }
-- asegura: { resultado = true ↔ todos los dıgitos de n son iguales }
-- }

todos_digitos_iguales :: Integer -> Bool
todos_digitos_iguales n |   n < 10 = True
                        |   mod n 10 == mod (div n 10) 10 = todos_digitos_iguales(div n 10)
                        |   otherwise = False