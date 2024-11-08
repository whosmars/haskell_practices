module Practica2 where

--Estas son funciones auxiliares que trabajan en tripletas que regresan los valores asociadas al nombre de ellas, estan sacadas del Manual de Haskell del curso
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

--Multiplica cada entrada de las tripletas respetando su orden, luego suma las 3 entradas
productoPunto3D :: (Int, Int, Int) -> (Int, Int, Int) -> Int 
productoPunto3D xs ys = (first xs * first ys) + (second xs * second ys) + (third xs * third ys)

--Multiplica cada entrada del n-producto punto, primero revisa que tengan la misma longitud y luego multiplica n entrada con n entrada de ambos vectores, hasta que ambos sean arreglos vacios, lo cual debe pasar porque tienen la misma longitud
productoPunto :: [Int] -> [Int] -> Int 
productoPunto xs ys
    | length xs /= length ys = error "Necesitamos listas del mismo largo"
productoPunto [] [] = 0 
productoPunto (x:xs) (y:ys) = (x*y) + productoPunto xs ys 

--Ocupo la formula que viene en el pdf que se subio al classroom, se obtiene una terna como salida
productoCruz :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
productoCruz xs ys = (prodCruzAux (second xs) (third ys) (third xs) (second ys), -1 * prodCruzAux (first xs) (third ys) (third xs) (first ys), prodCruzAux (first xs) (second ys) (second xs) (first ys) )

--facilita las cosas al momento de ocuparla en productoCruz, las 4 entradas se operan asi: ab - cd
prodCruzAux :: Int -> Int -> Int -> Int -> Int
prodCruzAux  a b c d = (a*b)-(c*d)  

--Se restan los vectores entrada por entrada, solo acepta vectores en R3
restaVectores :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
restaVectores xs ys = (first xs - first ys, second xs - second ys, third xs - third ys) 

--Segun un video de Matefacil, solo es obtener dos vectores a traves de la resta y luego operar el productoCruz del resultado
normalATriangulo :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
normalATriangulo xs ys zs = productoCruz (restaVectores xs ys) (restaVectores xs zs)


-- Funcion que recibe un entero y regresa una lista con sus divisores propios.
-- Un divisor propio de un número es cualquier divisor que no es el mismo
-- número que el que divide.
-- init hace que se quite la division con el mismo
divisoresPropios :: Int -> [Int]
divisoresPropios x
    | x < 0 = error "Solo esta definido para enteros positivos"
divisoresPropios x = init (reversaLista (divisoresPropiosAux x x))

--Itera con el mismo numero dos veces pero al de la derecha se le va restando uno, si el residuo de la division es cero, se concatena el numero en cuestion
divisoresPropiosAux :: Int -> Int -> [Int]
divisoresPropiosAux x 0 = []
divisoresPropiosAux x y 
    | mod x y == 0 = y: divisoresPropiosAux x (y-1)
    | otherwise =  divisoresPropiosAux x (y-1)

--devuelve la lista pero en el orden inverso, ocupado en divisoresPropios
reversaLista :: [Int] -> [Int]
reversaLista [] = []
reversaLista (x:xs) = reversaLista xs ++ [x]

--Lista infinita de numero pares
numerosPares = [2,4..]

-- manda a llamar a la funcion auxiliar y ocupa take para tomar los n numeros pares que se necesiten
-- Función recursiva que suma los primeros n numeros pares.
sumaPares :: Int -> Int
sumaPares 0 = 0
sumaPares x = sumaParesAux (take x numerosPares)

--Deberia llamarse algo como suma de listas, solo va sumando los elementos de la lista
sumaParesAux :: [Int] -> Int
sumaParesAux [] = 0
sumaParesAux (x:xs) = x + sumaParesAux xs


--Es primo solo comprueba si la lista de numeros divisibles es [1], lo cual significa que es primo, ya que arriba borramos la division por el mismo
-- Función para obtener el n-esimo numero primo
esPrimo :: Int -> Bool
esPrimo x 
    | divisoresPropios x == [1] = True
    |otherwise = False

--gracias a la funcion esPrimo se puede definir por comprension la lista de n numeros primos
numerosPrimos = [x | x <- [2..], esPrimo x]

--Trivial, solo manda a llamar a la lista por compresion, y last obtiene el ultimo elemento de la lista
primo :: Int -> Int
primo x = last (take x numerosPrimos)

--a traves dek residuo de dividir por 10, va obteniendo los digitos del numero
digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

--Deberia llamarse algo como suma de listas, solo va sumando los elementos de la lista
sumarDigitosLista :: [Int] -> Int
sumarDigitosLista [] = 0
sumarDigitosLista (x:xs) = x + sumarDigitosLista xs

--Solo manda a llamar a la funcion auxiliar digitos, que convierte el numero en arreglo, y sumarDigitos que solo suma los elementos de la lista
-- Función que suma los digitos de un numero
sumaDigitos :: Int -> Int
sumaDigitos x = sumarDigitosLista (digs x)

--Se considera el caso 0 debido a la implementacion de digs, en todo otro caso opera con normalidad, considera el caso negativo pasandolo a natural y concatenando el menos
imprimeUnEntero :: Int -> String 
imprimeUnEntero x
    |x == 0 = "0"
    |x > 0 = intToString (digs x)
    |otherwise ='-' : intToString (digs (-1*x))

--Conversion manual de los 10 digitos posibles a su representacion como char
intToString :: [Int] -> String
intToString [] = []
intToString (x:xs)
    | x == 0 = '0' : intToString xs
    | x == 1 = '1' : intToString xs
    | x == 2 = '2' : intToString xs
    | x == 3 = '3' : intToString xs
    | x == 4 = '4' : intToString xs 
    | x == 5 = '5' : intToString xs
    | x == 6 = '6' : intToString xs
    | x == 7 = '7' : intToString xs 
    | x == 8 = '8' : intToString xs
    | x == 9 = '9' : intToString xs