{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

--Ejercicio 2

eliminarRepetidos :: (Eq a) => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)    | elem x xs = eliminarRepetidos xs
                            | otherwise = x : eliminarRepetidos xs

ciudadesConectadasAux::AgenciaDeViajes->Ciudad->[Ciudad]
ciudadesConectadasAux [] _ = []
ciudadesConectadasAux ((origen,destino,duracion):xs) ciudad     | ciudad==origen = destino : ciudadesConectadasAux xs ciudad
                                                                | ciudad==destino = origen : ciudadesConectadasAux xs ciudad
                                                                | otherwise = ciudadesConectadasAux xs ciudad

ciudadesConectadas::AgenciaDeViajes->Ciudad->[Ciudad]
ciudadesConectadas agencia ciudad = eliminarRepetidos (ciudadesConectadasAux agencia ciudad)

--Ejercicio 5

hayVueloDirecto::AgenciaDeViajes->Ciudad->Ciudad->Bool
hayVueloDirecto [] _ _ = False
hayVueloDirecto ((a,b,duracion):xs) origen destino  | a==origen && b==destino = True
                                                    | otherwise = hayVueloDirecto xs origen destino

hayVueloConUnaEscala::AgenciaDeViajes->Ciudad->Ciudad->Bool
hayVueloConUnaEscala [] _ _ = False
hayVueloConUnaEscala ((a,b,duracion):xs) origen destino | a==origen = hayVueloDirecto xs b destino
                                                        | otherwise = hayVueloConUnaEscala xs origen destino

sePuedeLLegar::AgenciaDeViajes->Ciudad->Ciudad->Bool
sePuedeLLegar agencia origen destino = hayVueloDirecto agencia origen destino || hayVueloConUnaEscala agencia origen destino