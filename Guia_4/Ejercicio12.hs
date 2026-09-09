-- Ejercicio 12. Para n ∈ N se define la sucesion:
-- a▽n = 2 +   1                    (aparece n veces el 2).
--           -------
--            2 +   1
--               -------
--              .
--               .
--                .
--                 2 +    1
--                     -------
--                      2 + 1 / 2

-- Lo cual resulta en la siguiente definicion recursiva: a1 = 2, an = 2 + 1 /(an - 1)
-- Utilizando esta sucesion, especificar e implementar
-- una funcion raizDe2Aprox :: Integer ->Float que dado n ∈ N devuelva la aproximacion de √2 definida por √2 ≈ an −1.
-- Por ejemplo:
-- raizDe2Aprox 1 ⇝ 1
-- raizDe2Aprox 2 ⇝ 1,5
-- raizDe2Aprox 3 ⇝ 1,4

raiz_de_2_aprox :: Integer -> Float
raiz_de_2_aprox 1 = 2
raiz_de_2_aprox n = (2 + (1 / raiz_de_2_aprox(n-1))) - 1