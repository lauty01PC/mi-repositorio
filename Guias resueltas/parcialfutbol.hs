{--
1) Goles de no goleadores [1 punto]

problema golesDeNoGoleadores (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩, totalGolesTorneo: Z ): Z {
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {La suma de todos los elementos de goles es menor o igual a totalGolesTorneo}
    asegura: {res es la cantidad de goles convertidos en el torneo por jugadores que no son los goleadores de sus equipos}
--}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

golesDeNoGoleadores::[(String,String)]->[Int]->Int->Int
golesDeNoGoleadores goleadoresPorEquipo goles totalGolesTorneo = totalGolesTorneo - sumatoria goles

sumatoria::(Num t)=>[t]->t
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

{--
2) Equipos Válidos [3 puntos]

problema equiposValidos (goleadoresPorEquipo: seq⟨String x String⟩): Bool{
    requiere: {True}
    asegura: {(res = True) <-> goleadoresPorEquipo no contiene nombres de clubes repetidos, ni goleadores repetidos, ni jugadores con nombre de club}
--}

equiposValidos::[(String,String)]->Bool
equiposValidos xs = not (hayRepetidos (aplanar xs))

aplanar::[(t,t)]->[t]
aplanar [] = []
aplanar ((x,y):xs)= [x]++[y]++ aplanar xs

hayRepetidos::(Eq t)=>[t]->Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

pertenece::(Eq t)=>t->[t]->Bool
pertenece _ [] = False
pertenece a (x:xs)  | a==x = True
                    | otherwise = pertenece a xs

{--
3) Porcentaje de Goles [3 puntos]

problema porcentajeDeGoles (goleador: String, goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): R {
    requiere: {La segunda componente de algún elemento de goleadoresPorEquipo es goleador}
    requiere: {equiposValidos(goleadoresPorEquipo)}
    requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {Hay al menos un elemento de goles mayor estricto a 0}
    asegura: {res es el porcentaje de goles que marcó goleador sobre el total de goles convertidos por goleadores}
--}

--Para resolver este ejercicio pueden utilizar la siguiente función que devuelve como Float la división entre dos numeros de tipo Int:
division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b

porcentajeDeGoles::String->[(String,String)]->[Int]->Float
porcentajeDeGoles goleador goleadoresPorEquipo goles = division (cantGoles goleador goleadoresPorEquipo goles) (sumatoria goles)* 100

cantGoles::String->[(String,String)]->[Int]->Int
cantGoles j (x:xs) (y:ys)   | j==snd x = y
                            | otherwise = cantGoles j xs ys

{--
4) Botín de Oro [3 puntos]

problema botinDeOro (goleadoresPorEquipo: seq⟨String x String⟩, goles: seq⟨Z⟩): String {
    requiere: {equiposValidos(goleadoresPorEquipo)}
   requiere: {|goleadoresPorEquipo| = |goles|}
    requiere: {Todos los elementos de goles son mayores o iguales a 0}
    requiere: {|goles| > 0}
    asegura: {res es alguno de los goleadores de goleadoresPorEquipo que más tantos convirtió de acuerdo a goles}
--}

botinDeOro::[(String,String)]->[Int]->String
botinDeOro [j] [x] = snd j
botinDeOro (j1:j2:js) (x:y:xs)  | x>=y = botinDeOro (j1:js) (x:xs)
                                | otherwise = botinDeOro (j2:js) (y:xs)