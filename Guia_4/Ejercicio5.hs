-- Ejercicio 5. Implementar la funcion medioFact :: Integer ->Integer que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · .
-- problema medioFact (n: Z) : Z {
-- requiere: { n ≥ 0 }
-- asegura: { resultado =        [(n-1)/2]  
--                                   π    (n-2i) 
--                                 i = 0
-- }
-- Por ejemplo:
-- medioFact 10 ; 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 ; 3840.
-- medioFact 9 ; 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 ; 945.
-- medioFact 0 ; 1.

medio_fact :: Integer -> Integer
medio_fact 0 = 1
medio_fact 1 = 1
medio_fact n = n * medio_fact(n-2)