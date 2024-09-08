{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Distribution.Compat.CharParsing (digit)
--Ejercicio 1
fibonacci:: Integer->Integer
fibonacci x | x==0=0
            | x==1=1
            | otherwise=fibonacci (x-1) + fibonacci (x-2)

--Ejercicio 2

parteEntera::Float->Integer
parteEntera n | (n >= 0) && (n < 1) = 0
              | n >= 1 = parteEntera (n-1) + 1
              | n < 0 = parteEntera (n+1) - 1

--Ejercicio 3

{-Especificar e implementar la funcion esDivisible :: Integer ->Integer ->Bool que dados dos numeros
naturales determinar si el primero es divisible por el segundo. No esta permitido utilizar las funciones mod ni div.-}

esDivisible::Integer->Integer->Bool
esDivisible x y | x==0 = True
                | x<0 = False
                | otherwise= esDivisible (x-y) y

--Ejercicio 4
{-Especificar e implementar la funcion sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros
n numeros impares. Por ejemplo: sumaImpares 3 ⇝ 1+3+5 ⇝ 9.-}

sumaImpares :: Integer -> Integer
sumaImpares 1 = 1
sumaImpares n = n_esimoImpar + sumaImpares (n-1)
                where n_esimoImpar = 2*n-1

sumatoria::Int->Int
sumatoria   1 = 1
sumatoria   n = n + sumatoria (n-1)

productoria:: Int->Int
productoria 1 = 1
productoria n = n*productoria (n-1)

--Ejercicio 5

medioFact::Int->Int
medioFact n | n==0 || n==1 = 1
medioFact n = n*medioFact (n-2)

--Ejercicio 6 
sumaDigitos::Int->Int
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

--Ejercicio 7
todosDigitosIguales::Int->Bool
todosDigitosIguales n = n < 10 || mod n 10 == mod (div n 10) 10 && todosDigitosIguales (div n 10)

--Ejercicio 8
cantDigitos::Int->Int
cantDigitos n   | n < 10 = 1
                | otherwise= 1 + cantDigitos (div n 10)

iesimoDigito::Int->Int->Int
iesimoDigito n i =mod (div n (10^(cantDigitos n - i))) 10

--Ejercicio 9
sacarPrimerDigito :: Int->Int
sacarPrimerDigito n = mod n (10^(cantDigitos n - 1))

sacarUltimoDigito:: Int->Int
sacarUltimoDigito n = div n 10

quitarExtremos :: Int -> Int
quitarExtremos n = sacarPrimerDigito (sacarUltimoDigito n)

mostrarPrimerDigito :: Int -> Int
mostrarPrimerDigito n = div n (10^(cantDigitos n - 1))

mostrarUltimoDigito::Int->Int
mostrarUltimoDigito n = mod n 10

esCapicua :: Int->Bool
esCapicua n = n < 10 || ((mostrarUltimoDigito n==mostrarPrimerDigito n) && esCapicua (quitarExtremos n))

--Ejercicio 10
--a
f1:: Int->Int
f1 0 = 1
f1 n = 2^n + f1 (n-1)
--b
f2::Int->Float->Float
f2 1 q = q
f2 n q = q^n + f2 (n-1) q
--c
f3::Int->Float->Float
f3 n q = f2 (2*n) q
--d
f4::Int->Float->Float
f4 n q = f2 (2*n) q - f2 (n-1) q

--Ejercicio 11
sum1HastaN::Int->Int
sum1HastaN 0 = 0
sum1HastaN n = 1 + sum1HastaN (n-1)
factorial::Int->Int
factorial 0 = 1
factorial n = n*factorial (n-1)
eAprox::Int->Float
eAprox 0 = 1
eAprox n = 1/fromIntegral (factorial n) + eAprox (n-1)

e::Float
e = eAprox 10

--Ejercicio 12
a::Int->Float
a 1 = 2
a n = 2 + 1/(a(n-1))
raizDe2Aprox::Int->Float
raizDe2Aprox 1 = 1
raizDe2Aprox n = 1 + 1/(a(n-1))
