-- Ejercicio 6. Usando los siguientes tipos:
-- type Anio = Integer
-- type EsBisiesto = Bool
-- Programar la funcion bisiesto :: Anio -> EsBisiesto segun la siguiente especificacion:
-- problema bisiesto (anio : Z) : Bool {
-- requiere: {True}
-- asegura: {(res = false) ↔ (anio no es multiplo de 4, o bien, aino es multiplo de 100 pero no de 400)}
-- }
-- Por ejemplo:
-- bisiesto 1901 ⇝ False | bisiesto 1904 ⇝ True
-- bisiesto 1900 ⇝ False | bisiesto 2000 ⇝ True
type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto anio   |   mod anio 4 /= 0 = False
                |   mod anio 100 == 0 && mod anio 400 /= 0 = False
                |   otherwise = True