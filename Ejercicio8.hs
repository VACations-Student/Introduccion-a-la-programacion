import Data.Bool (otherwise)
-- Ejercicio 8. Implementar la funcion comparar :: Integer -> Integer -> Integer
-- problema comparar (a : Z, b : Z) : Z {
-- requiere: {True}
-- asegura:  { (res = 1) ↔ (sumaUltimosDosDigitos(a) < sumaUltimosDosDigitos(b))  }
-- asegura:  { (res = −1) ↔ (sumaUltimosDosDigitos(a) > sumaUltimosDosDigitos(b)) }
-- asegura:  { (res = 0) ↔ (sumaUltimosDosDigitos(a) = sumaUltimosDosDigitos(b))  }
-- }
-- problema sumaUltimosDosDigitos (x : Z) : Z {
-- requiere: {True}
-- asegura:  { res = (|x| mod 10) + ((div |x| 10) mod 10) }
-- }
-- Por ejemplo:
-- comparar 45 312 ⇝ -1 porque 45 ≺ 312 y 4 + 5 > 1 + 2.
-- comparar 2312 7 ⇝ 1 porque 2312 ≺ 7 y 1 + 2 < 0 + 7.
-- comparar 45 172 ⇝ 0 porque no vale 45 ≺ 172 ni tampoco 172 ≺ 45.

calcular_absoluto :: Integer -> Integer 
calcular_absoluto n
                | n >= 0 = n
                | otherwise = -n

suma_ultimos_dos_digitos :: Integer -> Integer
suma_ultimos_dos_digitos n = (calcular_absoluto n) `mod` 10 + ((div (calcular_absoluto n) 10) `mod` 10)

comparar :: Integer -> Integer -> Integer
comparar a b    |   (suma_ultimos_dos_digitos(a) < suma_ultimos_dos_digitos(b)) == True = 1
                |   (suma_ultimos_dos_digitos(a) > suma_ultimos_dos_digitos(b)) == True = -1
                |   otherwise = 0