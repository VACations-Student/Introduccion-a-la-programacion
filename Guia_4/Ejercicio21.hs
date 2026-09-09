-- Ejercicio 21. Especificar e implementar una funcion pitagoras :: Integer ->Integer ->Integer ->Integer que dados
-- m, n , r ∈ N≥0, cuente cuantos pares (p, q) con 0 ≤ p ≤ m y 0 ≤ q ≤ n satisfacen que p^2 + q^2 ≤ r^2. Por ejemplo:
-- pitagoras 3 4 5 ⇝ 20
-- pitagoras 3 4 2 ⇝ 6

probar_pares_con_n_fijo :: Integer -> Integer -> Integer -> Integer -> Integer
probar_pares_con_n_fijo n m r limite_superior_m
    |   m == limite_superior_m && (n^2 + m^2) > r^2 = 0
    |   m == limite_superior_m = 1
    |   (n^2 + m^2) > r^2 = 0
    |   (n^2 + m^2) <= r^2 = (1 + probar_pares_con_n_fijo n (m+1) r limite_superior_m)

probar_pares_con_n_variable :: Integer -> Integer -> Integer -> Integer -> Integer
probar_pares_con_n_variable n m r limite_superior_n
    |   n == limite_superior_n = probar_pares_con_n_fijo n 0 r m
    |   otherwise = (probar_pares_con_n_variable (n+1) m r limite_superior_n) + probar_pares_con_n_fijo n 0 r m


pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras n m r = probar_pares_con_n_variable 0 m r n