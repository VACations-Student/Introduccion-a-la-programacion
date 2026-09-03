-- Ejercicio 5. Implementar la funcion todosMenores :: (Integer, Integer, Integer) -> Bool
-- problema todosMenores (t : Z × Z × Z) : Bool {
-- requiere: {True}
-- asegura: {(res = true) ↔ ((f (t0) > g(t0)) ∧ (f (t1) > g(t1)) ∧ (f (t2) > g(t2)))}
-- }
-- problema f (n : Z) : Z {
-- requiere: {True}
-- asegura: {(n ≤ 7 → res = n**2) ∧ (n > 7 → res = 2*n − 1)}
-- }
-- problema g (n : Z) : Z {
-- requiere: {True}
-- asegura: {Si n es un numero par entonces res = n/2, en caso contrario, res = 3n + 1}
-- }

f :: Integer -> Integer
f n     |   n <= 7 = n * n
        |   otherwise = 2 * n - 1

g :: Integer -> Integer
g n     |   mod n 2 == 0 = div n 2
        |   otherwise = 3 * n + 1

todos_menores :: (Integer, Integer, Integer) -> Bool
todos_menores (n, m, k) = f n > g n && f m > g m && f k > g k