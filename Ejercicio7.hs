-- Ejercicio 7.
-- a) Implementar la funcion:
-- distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
-- problema distanciaManhattan (p : R × R × R, q : R × R × R) : R {
-- requiere: {True}
-- asegura: {res = Σ hasta 2 con i = 0  |pi − qi|}
-- }
-- Por ejemplo:
-- distanciaManhattan (2, 3, 4) (7, 3, 8) ⇝ 9
-- distanciaManhattan ((-1), 0, (-8.5)) (3.3, 4, (-4)) ⇝ 12.8
-- b) Reimplementar la funcion teniendo en cuenta el siguiente tipo: type Punto3D = (Float, Float, Float)
calcular_absoluto :: Float -> Float 
calcular_absoluto n
                | n >= 0 = n
                | otherwise = -n

distancia_manhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distancia_manhattan (a, b, c) (d, e, f) = calcular_absoluto(a - d) + calcular_absoluto(b - e) + calcular_absoluto(c - f)

type Punto3D = (Float, Float, Float)

re_distancia_manhattan :: Punto3D -> Punto3D -> Float
re_distancia_manhattan (a, b, c) (d, e, f) = calcular_absoluto(a - d) + calcular_absoluto(b - e) + calcular_absoluto(c - f)