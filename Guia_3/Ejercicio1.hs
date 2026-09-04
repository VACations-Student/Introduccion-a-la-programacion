-- a) implementar la funcion parcial f :: Integer -> Integer definida por extension de la siguiente manera:
-- f (1) = 8
-- f (4) = 131
-- f (16) = 16

-- a)
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- b) Analogamente, especificar e implementar la funcion parcial g :: Integer -> Integer
-- g(8) = 16
-- g(16) = 4
-- g(131) = 1

-- b)
g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

-- c) A partir de las funciones definidas en los ıtems a) y b), implementar las funciones parciales h = f ◦ g y k = g ◦ f

-- c)
h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)