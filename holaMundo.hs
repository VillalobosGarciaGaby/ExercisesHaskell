module Ejemplo where 
    
tuFechaDeNacimiento = 1987

tuNombre =  "Gaby"

doble x = 2 * x

dobleDeXMasUno x = doble x + 1

nextNum n = n + 1

square n = n * n

suma:: Int -> Int -> String

suma a b = show(a + b)

cadena = "Hola chetos"

a = 'a'

{-
-- suma:: Int -> Int -> Int 
suma a b c =  a +b +c
-}


esPar ::  Int  -> Bool
esPar n = mod n 2 == 0

sumaGrande :: Integer -> Integer -> Integer

sumaGrande a b = a + b
