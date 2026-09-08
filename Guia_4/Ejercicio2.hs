-- Ejercicio 2. Implementar una funcion parteEntera :: Float ->Integer segun la siguiente especificacion:
-- problema parteEntera (x: R) : Z {
-- requiere: { x ≥ 0 }
-- asegura: { resultado ≤ x < resultado + 1 }
-- }
parte_entera :: Float -> Integer
parte_entera n  |   n < 1 && n > 0 = 0
                |   otherwise = 1 + parte_entera(n-1)