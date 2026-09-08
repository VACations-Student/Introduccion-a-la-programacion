-- Ejercicio 4. Especificar e implementar la funcion sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros
-- n numeros impares. Por ejemplo: sumaImpares 3 ; 1+3+5 ⇝ 9.

suma_impares :: Integer -> Integer
suma_impares 1 = 1
suma_impares n = 2 * n - 1 + suma_impares (n - 1)