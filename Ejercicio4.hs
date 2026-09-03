--Ejercicio 4. ⋆ Especificar e implementar las siguientes funciones utilizando tuplas para representar pares y ternas de numeros.
-- a) productoInterno: calcula el producto interno entre dos tuplas de R × R.
-- Especificacion:
-- producto_interno ( A = { x ∈ R**2 }, B = { y ∈ R**2 } ) : R
-- requiere: { #A == #B }
-- asegura:  { res = fst a * fst b + snd a * snd b }
producto_interno :: (Double, Double) -> (Double, Double) -> Double
producto_interno a b = fst a * fst b + snd a * snd b

-- esParMenor: dadas dos tuplas de R × R, decide si cada coordenada de la primera tupla es menor a la coordenada
-- correspondiente de la segunda tupla.
-- Especificacion:
-- es_par_menor ( A = { x ∈ R**2 }, B = { y ∈ R**2 } ) : Bool
-- requiere: { #A == #B }
-- asegura:  { fst a < fst b && snd a < snd b }
es_par_menor :: (Double, Double) -> (Double, Double) -> Bool
es_par_menor a b = fst a < fst b && snd a < snd b

-- c) distancia: calcula la distancia euclıdea entre dos puntos de R2.
-- Especificacion:
-- distancia ( A = { x ∈ R**2 }, B = { y ∈ R**2 } ) : R
-- requiere: { #A == #B }
-- asegura:  { res = sqr((fst b - fst a)**2 + (snd b - snd a)**2 ) }
distancia :: (Double, Double) -> (Double, Double) -> Double
distancia a b = sqrt((fst b - fst a)**2 + (snd b - snd a)**2)

-- d) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
-- Especificacion:
-- suma_terna ( A = { x ∈ Z**3 } ) : Z
-- requiere: { TRUE }
-- asegura:  { res = a + b + c }
suma_terna :: (Integer, Integer, Integer) -> Integer
suma_terna (a, b, c) = a + b + c

-- e) sumarSoloMultiplos: dada una terna de numeros enteros y un natural, calcula la suma de los elementos de la terna que
-- son multiplos del numero natural.
-- Por ejemplo:
-- sumarSoloMultiplos (10,-8,-5) 2 ⇝ 2
-- sumarSoloMultiplos (66,21,4) 5 ⇝ 0
-- sumarSoloMultiplos (-30,2,12) 3 ⇝ -18
-- Especifiacion:
-- sumar_solo_multiplos ( A = { x ∈ Z**3 }, n ∈ N ) : Z
-- requiere: { n > 0 }
-- asegura:  { (a == n * k && b == n * k && c == n * k → res = a + b + c) ∨ (a == n * k && b == n * k → res = a + b ) 
-- ∨ (a == n * k && c == n * k → res = a + c ) ∨ (b == n * k && c == n * k → res = b + c ) 
-- ∨ (a /= n * k && b /= n * k && c /= n * k → res = 0 ) }
sumar_solo_multiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumar_solo_multiplos (a, b, c) n    |   mod a n == 0 && mod b n == 0 && mod c n == 0 = a + b + c
                                    |   mod a n == 0 && mod b n == 0 = a + b
                                    |   mod a n == 0 && mod c n == 0 = a + c
                                    |   mod b n == 0 && mod c n == 0 = b + c
                                    |   otherwise = 0

-- f) posPrimerPar: dada una terna de enteros, devuelve la posicion del primer numero par si es que hay alguno, o devuelve
-- 4 si son todos impares.
-- Especifiacion:
-- pos_primer_par ( A = { x ∈ Z**3 } )
-- requiere: { TRUE }
-- asegura:  { (a == 2 * k → res = 1) ∨ (b == 2 * k → res = 2) ∨ (c == 2 * k → res = 3) ∨ (otherwise = 4) }
pos_primer_par :: (Integer, Integer, Integer) -> Integer
pos_primer_par (a, b, c)    |   mod a 2 == 0 = 1
                            |   mod b 2 == 0 = 2
                            |   mod c 2 == 0 = 3
                            |   otherwise = 4

-- g) crearPar :: a -> b -> (a, b): a partir de dos componentes, crea un par con esos valores. Debe funcionar para elementos de cualquier tipo.
-- Especifiacion:
-- crear_par ( a : let, b : let ) : (let, let)
-- requiere: { TRUE }
-- asegura:  { res = (a, b) }
crear_par :: a -> b -> (a, b)
crear_par a b = (a, b)

--h) invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como parametro. Debe funcionar para elementos de cualquier tipo
-- Especifiacion:
-- invertir ( (a , b) ) : (let, let)
-- requiere: { TRUE }
-- asegura:  { res = (b, a) }
invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

--i) Reescribir los ejercicios productoInterno, esParMenor y distancia usando el siguiente renombre de tipos:
type Punto2D = (Float, Float)

-- re_producto_interno: calcula el producto interno entre dos Punto2D
-- Especificacion:
-- re_producto_interno ( A : Punto2D, B : Punto2D ) : R
-- requiere: { TRUE }
-- asegura:  { res = fst a * fst b + snd a * snd b }
re_producto_interno :: Punto2D -> Punto2D -> Float
re_producto_interno a b = fst a * fst b + snd a * snd b

-- esParMenor: dados 2 elementos Punto2D, decide si cada coordenada del primer Punto2D es menor a la coordenada
-- correspondiente del segundo Punto2D
-- Especificacion:
-- es_par_menor ( A : Punto2D, B : Punto2D ) : Bool
-- requiere: { TRUE }
-- asegura:  { res = fst a < fst b && snd a < snd b }
re_es_par_menor :: Punto2D -> Punto2D -> Bool
es_par_menor a b = fst a < fst b && snd a < snd b

-- re_distancia: calcula la distancia euclıdea entre las coordenadas de 2 Punto2D
-- Especificacion:
-- re_distancia ( A : Punto2D, B : Punto2D ) : R
-- requiere: { TRUE }
-- asegura:  { res = sqr((fst b - fst a)**2 + (snd b - snd a)**2 ) }
re_distancia :: Punto2D -> Punto2D -> Float
distancia a b = sqrt((fst b - fst a)**2 + (snd b - snd a)**2)