{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
--Ejercicio 1

porcentajeDeVotosAfirmativos::[(String,String)]->[Int]->Int->Float
porcentajeDeVotosAfirmativos formulas votos cantTotalVotos = division (sumatoria votos) cantTotalVotos * 100

sumatoria::(Num t)=>[t]->t
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

division::Int->Int->Float
division a b = fromIntegral a / fromIntegral b

--Ejercicio 2

formulasInvalidas::[(String,String)]->Bool
formulasInvalidas [] = False
formulasInvalidas ((x,y):xs) = x==y || perteneceTuplas x xs || perteneceTuplas y xs

perteneceTuplas::(Eq t)=>t->[(t,t)]->Bool
perteneceTuplas a [] = False
perteneceTuplas a ((x,y):xs)    | a==x = True
                                | a==y = True
                                | otherwise= perteneceTuplas a xs

--Ejercicio 3
porcentajeDeVotos::String->[(String,String)]->[Int]->Float
porcentajeDeVotos vice formulas votos = division (votosVice vice formulas votos) (sumatoria votos)* 100

votosVice::String->[(String,String)]->[Int]->Int
votosVice v xs ys = elementoIesimo (posicionVice v xs) ys

elementoIesimo::Int->[t]->t
elementoIesimo 0 xs = head xs
elementoIesimo n xs = elementoIesimo (n - 1) (tail xs)

posicionVice::(Eq t)=>t->[(t,t)]->Int
posicionVice v ((x,y):xs)   | v==y = 0
                            | otherwise = 1 + posicionVice v xs

--Ejercicio 4

minimo::[Int]->Int
minimo [x] = x
minimo (x:xs) | x < minimo xs = x
              | otherwise = minimo xs

posicionDe::(Eq t)=>t->[t]->Int
posicionDe a (x:xs) | a==x = 0
                    |otherwise = 1 + posicionDe a xs

menosVotadoAux::[(String,String)]->[Int]->(String,String)
menosVotadoAux formula votos = elementoIesimo (posicionDe (minimo votos) votos) formula

menosVotado::[(String,String)]->[Int]->String
menosVotado formula votos = presidente (menosVotadoAux formula votos)

presidente::(String,String)->String
presidente (x,y) = x 