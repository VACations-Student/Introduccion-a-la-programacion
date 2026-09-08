-- Ejercicio 7. Implementar la funcion iesimoDigito :: Integer ->Integer ->Integer que dado un n ∈ Z mayor o igual
-- a 0 y un i ∈ Z mayor o igual a 1 y menor o igual a la cantidad de dıgitos de n, devuelve el i-esimo dıgito de n.

-- problema iesimoDigito (n: Z, i: Z) : Z {
-- requiere: { n ≥ 0 ∧ 1 ≤ i ≤ cantDigitos(n) }
-- asegura: { resultado = (n div 10**(cantDigitos(n)−i)) mod 10 }
-- }

-- problema cantDigitos (n: Z) : N {
-- requiere: { n ≥ 0 }
-- asegura: { n = 0 → resultado = 1}
-- asegura: { n̸ = 0 → (n div 10**resultado−1 > 0 ∧ n div 10**resultado = 0) }
-- }

cant_digitos :: Integer -> Integer
cant_digitos 0 = 1
cant_digitos n  |   n < 10 = 1
                |   otherwise = 1 + cant_digitos(div n 10)

iesimo_digito :: Integer -> Integer -> Integer
iesimo_digito n i = mod (div n (10^(cant_digitos n-i))) 10