--Ejercicio 4. ⋆ Especificar e implementar las siguientes funciones utilizando tuplas para representar pares y ternas de numeros.
-- a) productoInterno: calcula el producto interno entre dos tuplas de R × R.
-- Especificacion:
-- producto_interno ( A = { x ∈ R }, B = { y ∈ R } ) : C = { x ∈ A ∧ y ∈ B }
-- requiere: { #A == #B }
-- asegura: { res = { x ∈ A ∧ y ∈ B } }
producto_interno :: (Double, Double) -> (Double, Double) -> Double
producto_interno a b = fst a * fst b + snd a * snd b