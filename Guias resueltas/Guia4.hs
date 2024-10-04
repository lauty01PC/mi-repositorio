{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use even" #-}
--Ejercicio 1
fibonacci:: Int->Int
fibonacci x | x==0=0
            | x==1=1
            | otherwise=fibonacci (x-1) + fibonacci (x-2)

--Ejercicio 2

parteEntera::Float->Int
parteEntera n | n >= 0 && n < 1 = 0
              | n >= 1 = parteEntera (n-1) + 1
              | n < 0 = parteEntera (n+1) - 1

--Ejercicio 3

{-Especificar e implementar la funcion esDivisible :: Int ->Int ->Bool que dados dos numeros
naturales determinar si el primero es divisible por el segundo. No esta permitido utilizar las funciones mod ni div.-}

esDivisible::Int->Int->Bool
esDivisible x y | x==0 = True
                | x<0 = False
                | otherwise= esDivisible (x-y) y

--Ejercicio 4
{-Especificar e implementar la funcion sumaImpares :: Int ->Int que dado n ∈ N sume los primeros
n numeros impares. Por ejemplo: sumaImpares 3 ⇝ 1+3+5 ⇝ 9.-}

sumaImpares :: Int -> Int
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
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n*medioFact (n-2)

--Ejercicio 6 

sumDigitos :: Int -> Int
sumDigitos 0 = 0
sumDigitos n = mod n 10 + sumDigitos (div n 10)

{-
Ejercicio 7. Implementar la funcion todosDigitosIguales :: Int ->Bool que determina si todos los digitos de un numero natural son iguales, es decir:
problema todosDigitosIguales (n: Z) : B 
{requiere: { n > 0 }
asegura: { resultado = true ↔ todos los dıgitos de n son iguales }
}
-}
todosDigitosIguales:: Int->Bool
todosDigitosIguales n   | n < 10 = True
                        | otherwise = mod n 10 == div (mod n 10) 10 && todosDigitosIguales (div n 10)

--Ejercicio 8
nDigito::Int->Int->Int
nDigito n x = mod (div x (10^(n-1))) 10

cantDigitos ::Int->Int
cantDigitos 0 = 1
cantDigitos n = 1 + cantDigitos (div n 10)

iEsimoDigito:: Int->Int->Int
iEsimoDigito n i = mod (div n (10^(cantDigitos n - i))) 10

{-Ejercicio 9. Especificar e implementar una funcion esCapicua :: Int ->Bool que dado n ∈ N≥0 determina si n es
un numero capicua.-}

numDigitos :: Int -> Int
numDigitos n    | n>=0 && n< 10 = 1
                | otherwise = 1 + numDigitos (div n 10)

quitarExtremos:: Int -> Int
quitarExtremos n = mod (div n 10) (10 ^ (numDigitos n - 2))

primerDigito :: Int -> Int
primerDigito n  | n < 10    = n
                | otherwise = primerDigito (div n 10)

compararDigitos :: Int -> Bool
compararDigitos n   | n < 10    = True
                    | n < 100   = mod n 10 == div n 10
                    | otherwise = mod n 10 == primerDigito n && compararDigitos (quitarExtremos n)


--Ejercicio 10 
--a
f1::Int->Int
f1 0 = 1
f1 n = 2^n + f1 (n-1)
--b

f2 :: Int->Float->Float
f2 n q  | n <= 0 = 0
        | otherwise = q^n + f2 (n-1) q
--c
f3::Int->Float->Float
f3 n = f2 (2*n)
--d
f4 :: Int -> Float -> Float
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
a n = 2 + 1/a (n-1)
raizDe2Aprox::Int->Float
raizDe2Aprox 1 = 1
raizDe2Aprox n = 1 + 1/a (n-1)

--Ejercicio 13

sumatoriaInterna:: Int->Int->Int
sumatoriaInterna _ 0 = 0
sumatoriaInterna n j = n^j + sumatoriaInterna n (j-1)

sumatoriaDoble::Int->Int->Int
sumatoriaDoble 0 _ = 0
sumatoriaDoble n m = sumatoriaDoble (n-1) m + sumatoriaInterna n m

--Ejercicio 14

potencias :: Int->Int->Int
potencias q 1 = q
potencias q n = q ^ n * potencias q (n - 1)

sumaPotencias :: Int->Int->Int->Int
sumaPotencias q n m = potencias q n * potencias q m

--Ejercicio 15

sumaRacInterna::Int->Int->Float
sumaRacInterna n 1 = fromIntegral n
sumaRacInterna n q = fromIntegral n / fromIntegral q + sumaRacInterna n (q-1)

sumaRacionales::Int->Int->Float
sumaRacionales 0 m = 0
sumaRacionales n m = sumaRacionales (n-1) m + sumaRacInterna n m

--Ejercicio 16
--a

menorDivisor :: Int->Int
menorDivisor n  |n == 1 = 1
                |n == 2 = 2
                | otherwise = menorDivisorDesde n 2

menorDivisorDesde :: Int->Int->Int
menorDivisorDesde n i | mod n i == 0 = i
                      | otherwise = menorDivisorDesde n (i + 1)

--b

esPrimo::Int->Bool
esPrimo n = menorDivisor n == n

--c
maximoDivisor::Int->Int
maximoDivisor n = mayorDivisorDesde n (n-1)

mayorDivisorDesde::Int->Int->Int
mayorDivisorDesde n 0 = 0
mayorDivisorDesde n i   | mod n i == 0 = i
                        | otherwise= mayorDivisorDesde n (i-1)

maximoComunDivisor::Int->Int->Int
maximoComunDivisor n 0 = n
maximoComunDivisor n m = maximoComunDivisor m (mod n m)

sonCoprimos::Int->Int->Bool
sonCoprimos n m = maximoComunDivisor m n ==1

sonCoprimosV2 :: Int->Int->Bool
sonCoprimosV2 x y
    | x == 1 || y == 1 = True
    | esPrimo x && esPrimo y = True
    | mod x y == 0 || mod y x == 0 = False
    | menorDivisor x == menorDivisor y = False
    | menorDivisor x /= menorDivisor y = True
    | otherwise = False

--d 
nEsimoPrimo :: Int->Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = proximoPrimo (nEsimoPrimo (n - 1))

proximoPrimo :: Int->Int
proximoPrimo n | esPrimo (n + 1) = n + 1
               | otherwise = proximoPrimo (n + 1)

--Ejercicio 17

esFibonacci::Int->Bool
esFibonacci n = encontrarFibonacci n 0

encontrarFibonacci::Int->Int->Bool
encontrarFibonacci n i  | fibonacci i == n = True
                        | n<fibonacci i = False
                        | otherwise = encontrarFibonacci n (i+1)

--Ejercicio 18

f18::Int->Int
f18 n   | (0 <=n) && (n < 10) && (mod n 2 == 0) = n
        | (mod (primerDigito n) 2 == 0) && (mod (ultimoDigito n) 2 ==0) && (primerDigito n >= ultimoDigito n) = f18 (sacarUltimoDigito n)
        | (mod (primerDigito n) 2 == 0) && (mod (ultimoDigito n) 2 ==0) && (primerDigito n < ultimoDigito n) = f18 (sacarPrimerDigito n)
        | (mod (primerDigito n) 2 == 0) && (mod (ultimoDigito n) 2 ==1) = f18 (sacarUltimoDigito n)
        | (mod (primerDigito n) 2 == 1) && (mod (ultimoDigito n) 2 ==0) = f18 (sacarPrimerDigito n)
        | (mod (primerDigito n) 2 == 1) && (mod (ultimoDigito n) 2 ==1) = f18 (quitarExtremos n)
        | otherwise = -1

sacarPrimerDigito::Int->Int
sacarPrimerDigito n = mod n (10 ^ (numDigitos n -1))

sacarUltimoDigito::Int->Int
sacarUltimoDigito n = div n 10

ultimoDigito::Int->Int
ultimoDigito n = mod n 10

--Ejercicio 19

sumaPrimosHasta :: Int->Int
sumaPrimosHasta m | m == 1 = 2
                  | otherwise = nEsimoPrimo m + sumaPrimosHasta (m - 1)

esSumaPrimosHasta :: Int->Int-> Bool
esSumaPrimosHasta m n | sumaPrimosHasta m == n = True
                      | sumaPrimosHasta m > n = False
                      | otherwise = esSumaPrimosHasta (m + 1) n

esSumaInicialDePrimos :: Int-> Bool
esSumaInicialDePrimos n = esSumaPrimosHasta 1 n

--Ejercicio 20
sumaTodosDivisoresAux::Int->Int->Int
sumaTodosDivisoresAux n i       | i==n =i
                                | mod n i == 0 = i + sumaTodosDivisoresAux n (i+1)
                                | otherwise = sumaTodosDivisoresAux n (i+1)

sumaTodosDivisores::Int->Int
sumaTodosDivisores n = sumaTodosDivisoresAux n 1

tomaValorMax::Int->Int->Int
tomaValorMax n1 n2      | n1==n2 = n1
                        | sumaTodosDivisores n1 >= sumaTodosDivisores n2 = tomaValorMax n1 (n2-1)
                        | sumaTodosDivisores n1 < sumaTodosDivisores n2 = tomaValorMax (n1+1) n2

--Ejercicio 21

-- Función principal que inicia el conteo de los pares (p, q)
pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras m n r = contarPares m n r 0 0

-- Función recursiva que cuenta los pares (p, q) que cumplen p^2 + q^2 <= r^2
contarPares :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
contarPares m n r p q
    | p > m                 = 0  -- Caso base: cuando p supera el límite m, terminamos la recursión
    | q > n                 = contarPares m n r (p + 1) 0  -- Cuando q supera el límite n, reiniciamos q y pasamos al siguiente p
    | p^2 + q^2 <= r^2      = 1 + contarPares m n r p (q + 1)  -- Si el par cumple la condición, sumamos 1 y continuamos
    | otherwise             = contarPares m n r p (q + 1)  -- Si no cumple la condición, simplemente continuamos

--Descomponer en primos

factoresPrimos :: Int -> [Int]
factoresPrimos n = descomponer n 2  

descomponer :: Int -> Int -> [Int]
descomponer 1 _ = []  
descomponer n divisor   | mod n divisor == 0 = divisor : descomponer (div n divisor) divisor
                        | otherwise = descomponer n (divisor + 1)
