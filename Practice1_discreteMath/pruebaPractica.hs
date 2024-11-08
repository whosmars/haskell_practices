data Nat = Cero | Suc Nat deriving(Show,Eq)

--1.Define las siguientes listas utilizando listas por comprension (lista de numeros naturales, lista de multiplos de diez y numeros primeros primos (esta ultima no la implemente o.0 ))):
listaNumerosNat = [1..]
listaMultiplosDiez = [10 * x | x <- [1..]]

--2.Define la funcion natToInt, la cual debe recibir un numero natural n y devuelve el mismo como un entero
--La funcion principal solo manda a llamar a la auxiliar la cual tiene un "contador" al que se le suma una y se resta uno en su otra variable Nat
natToInt :: Nat -> Int
natToInt n = natToIntAux n 0

natToIntAux:: Nat -> Int -> Int
natToIntAux Cero n = n
natToIntAux (Suc n) m = natToIntAux n (m+1)

--3.Define la funcion intToNat, la cual debe recibir un numero entero y devuelve el mismo como un natural.
--Igual que arriba pero ahora la se le suma un sucesor en el auxiliar y se le resta uno al Int
intToNat :: Int -> Nat
intToNat n = intToNatAux n Cero

intToNatAux :: Int -> Nat -> Nat
intToNatAux 0 n = n
intToNatAux n m = intToNatAux (n-1) (Suc m)

--4.Define la funcion combinaciones la cual recibe dos n ́umeros naturales n y m y devuelve el numero de combinaciones de n elementos tomados de m en m, es decir:
-- Literalmente ocupa la formula de la combinacion, se necesaitaron todas las operaciones ya definidas en clase y se agregaron nuevas, las cuales explicare
combinaciones :: Nat -> Nat -> Nat
combinaciones n Cero = Suc Cero
combinaciones Cero n = Cero
combinaciones n m = divisionNat (factNat n) (prodNat (factNat m) (factNat(restaNat n m))) 

prodNat :: Nat -> Nat -> Nat
prodNat n Cero = Cero
prodNat n (Suc m) = sumaNat n (prodNat n m)

sumaNat :: Nat -> Nat -> Nat
sumaNat n Cero = n
sumaNat n (Suc m) = Suc (sumaNat n m)

factNat::Nat -> Nat
factNat Cero = Suc Cero
factNat (Suc n) = prodNat (Suc n) (factNat n)

--Por cada llamada se resta uno por igual al primer y al segundo argumento Nat
restaNat :: Nat -> Nat -> Nat
restaNat Cero n = Cero
restaNat n Cero = n
restaNat (Suc n) (Suc m) = restaNat n m

--Funciona gracias a la resta definida arriba, tiene problemas si el resultado no es Nat, sumandole uno sucesor mas
divisionNat :: Nat -> Nat -> Nat
divisionNat n Cero = Cero
divisionNat Cero n = Cero
divisionNat n m = divisionNatAux n m Cero

divisionNatAux :: Nat -> Nat -> Nat -> Nat
divisionNatAux Cero m p = p
divisionNatAux n m p = divisionNatAux (restaNat n m) m (Suc p)

--5.Define la funcion mcd la cual recibe dos numeros naturales y devuelve como resultado su maximo comun divisor.
--Se hace una conversion a los int y se ocupa el algoritmo que relaciona el residuo y el cociente con mcd, se hace recursion hasta que el divisor o el dividendo es cero, lo cual termina el algoritmo
mcd :: Nat -> Nat -> Nat
mcd n Cero = Cero
mcd Cero m = Cero
mcd n m = mcdAux (natToInt n) (natToInt m)

mcdAux :: Int -> Int -> Nat
mcdAux n 0 = intToNat n
mcdAux 0 m = intToNat m
mcdAux n m = mcdAux m (rem n m)


--6.Define la funcion mcm la cual recibe dos numeros naturales y devuelve como resultado su minimo comun multiplo.
--Multiplica a los dos nat y luego los divide entre su mcd
mcm :: Nat -> Nat -> Nat
mcm n Cero = Cero
mcm Cero m = Cero
mcm n m = divisionNat (prodNat n m) (mcd n m)


--7. Define la funcion fibonacci, la cual debe recibir un numero natural n y devuelve el n-esimo elemento de la serie de Fibonacci.
--Se define como 0 y 1 los primeros terminos, lo cual es erroneo pero funciona para la implementacion, lo demas literal se define casi como la formula matematica de fibonacci
fibonacci :: Nat -> Nat
fibonacci n = fibonacciAux n Cero (Suc Cero) 

fibonacciAux :: Nat -> Nat -> Nat -> Nat
fibonacciAux Cero m p = p
fibonacciAux (Suc n) m p = fibonacciAux n p (sumaNat m p)   