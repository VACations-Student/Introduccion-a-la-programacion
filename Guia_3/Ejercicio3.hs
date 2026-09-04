-- Ejercicio 3. Implementar una funcion estanRelacionados :: Integer -> Integer -> Bool
-- problema estanRelacionados (a : Z, b : Z) : Bool {
-- requiere: {a̸ = 0 ∧ b̸ = 0}
-- asegura: {(res = true) ↔ (a ∗ a + a ∗ b ∗ k = 0 para alg´un k ∈ Z con k̸ = 0)}
-- }
-- Por ejemplo:
-- estanRelacionados 8 2 ⇝ True porque existe k = −4 tal que 8 * 8 + 8 × 2 × (−4) = 0
-- estanRelacionados 7 3 ⇝ False porque no existe un k entero tal que 7 * 7 + 7 × 3 × k = 0

estan_relacionados :: Integer -> Integer -> Bool
estan_relacionados n m = n * n + n * m * (div(-n*n) (n*m)) == 0