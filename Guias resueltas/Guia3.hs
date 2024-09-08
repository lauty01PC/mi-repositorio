{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
import Distribution.Simple.Setup (trueArg)
import Graphics.Win32 (vK_PLAY)
import Language.Haskell.TH (Lit(IntegerL))
-- Ejercicio 1

-- a
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

-- b

g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1

-- c
h:: Integer->Integer
h x = g (f x)

k::Integer->Integer
k x = g (f x)

-- Ejercicio 2
-- a
{-Especificacion:
problema absoluto (x:Z) : Z {Asegura {res= |x|}}
-}
absoluto :: Integer->Integer
absoluto x | x >= 0 = x
           | otherwise = -x

--b 
{-Especificacion:
problema maximoAbsoluto (x:Z,y:Z) : Z {Asegura {res=max {|x|,|y|}}
-}
maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y | absoluto x >= absoluto y = absoluto x
                   | otherwise = absoluto y

--c
{-Especificacion:
problema maximo3 (x:Z,y:Z,z:Z): Z {Asegura {res=max {x,y,z}}}
-}
maximo2 :: Integer->Integer->Integer
maximo2 x y |x>=y=x
            |otherwise=y

maximo3 :: Integer->Integer->Integer->Integer
maximo3 x y z   | maximo2 x y >=z = maximo2 x y
                | otherwise= z

--d 
{-Especificacion:
problema algunoEs0 (x:Q,y:Q):Bool {Asegura:res=true <-> x=0 v y= 0}
-}

algunoEs0V1 :: Float->Float->Bool
algunoEs0V1 x y | x==0 = True
                | y==0 = True
                | otherwise = False

algunoEs0V2 :: Float->Float->Bool
algunoEs0V2 x y | x==0 || y==0 = True

algunoEs0PM :: Float->Float->Bool
algunoEs0PM 0 _ = True
algunoEs0PM _ 0 = True
algunoEs0PM _ _ = False

--e
{-Especificacion:
problema ambosSon0 (x:Q,y:Q):Bool {Asegura: res=true <-> x = 0 ^ y = 0}
-}

ambosSon0 :: Float->Float->Bool
ambosSon0 x y   | x==0 && y==0 = True
                | otherwise = False

ambosSon0PM :: Float->Float->Bool
ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False

--f 
{-Especificacion:
problema mismoIntervalo (x:R,y:R):Bool {Asegura: res=true <-> ((x<=3 ^ y <=3) v (x,y ∈ (3,7]) v (x > 7 ^ y>7))} 
-}
mismoIntervalo :: Float->Float->Bool
mismoIntervalo x y  | x<=3 && y<=3 = True
                    | (x>3 && x<=7) && y>3 && y<=7= True
                    | x>7 && y>7 = True
                    | otherwise = False

--g
{-Especificacion:
problema sumaDistintos (x:Z,y:Z,z:Z):Z {Asegura: res=x+y+z <-> x≠y≠z}
-}
sumaDistintos :: Integer->Integer->Integer->Integer
sumaDistintos x y z | x==y || y==z || x==z = undefined
sumaDistintos x y z = x + y + z

--h
{-Especificacion:
problema esMultiploDe (x:Z;y:Z):Bool {Requiere:{x>=0 ^ y>=0} Asegura:{res=True <-> x=y*k, k>0 ∈ Z}} 
-}
esMultiploDe :: Integer->Integer->Bool
esMultiploDe x y = mod x y == 0

--i
{-Especificacion:
problema digitoUnidades (x:Z) : Z {Asegura: {res= x mod 10}}
-}
digitoUnidades :: Integer->Integer
digitoUnidades x = mod x 10

--j
{-Especificacion:
problema digitoDecenas (x:Z) : Z {Asegura: {res= (div x 10) mod 10}}
-}
digitoDecenas :: Integer -> Integer
digitoDecenas x = digitoUnidades (div x 10)

--Ejercicio 3

{- Especificacion:
problema estanRelacionados (a:Z, b:Z) : Bool { requiere: {a≠0 ∧ b≠0} asegura: {(res = true) ↔ a^2+b*a*k=0 para algun k ∈ Z con k≠0)}
-}
{- Simplificacion: a^2+b*a*k=0 <-> a(a+bk)=0 <-> a+b*k=0 <-> a=b*q con q=-k ∈ Z <-> a=0 (b)
-}
estanRelacionados :: Integer->Integer->Bool
estanRelacionados a b = mod a b == 0

--Ejercicio 4

--a
{-Especificacion: 
problema prodInt (u:(RxR);v:(RxR)): R {Asegura:{res=u_0*v_0+u_1*v_1 }}
-}
prodInt :: (Float,Float)->(Float,Float)->Float
prodInt (ux,uy) (vx,vy) = ux*vx+uy*vy

--b
{-Especificacion:
problema todoMenor (u:(RxR);v:(RxR)): Bool {Asegura: {res=True <-> u_0<v_1 ^ u_1<v_1}}
-}
todoMenor::(Float,Float)->(Float,Float)->Bool
todoMenor (ux,uy) (vx,vy)   |ux<vx && uy<vy = True
                            | otherwise = False

--c
{-Especificacion:
problema distanciaPuntos: (u:(RxR);v:(RxR)): R {Asegura:{res=sqrt((u_0-v_0)^2+(u_1-v_1)^2)}} 
-}
distanciaPuntos::(Float,Float)->(Float,Float)->Float
distanciaPuntos (ux,uy) (vx,vy) = sqrt ((ux-vx)^2+(uy-vy)^2)

--d
{-Especificacion:
problema sumaTerna: (u:(ZxZxZ)): Z {Asegura: {res=u_0+u_1+u_2}}
-}
sumaTerna:: (Integer,Integer,Integer)->Integer
sumaTerna (ux,uy,uz) = ux+uy+uz

--e
{-Especificacion:
problema sumarSoloMultiplos: (u:(ZxZxZ),n:Z): Z {Requiere:{n>0} 
Asegura: {Se suman las coordenadas que sean multiplos de n}}
-}

multiploDevuelveMultiplo :: Integer->Integer->Integer
multiploDevuelveMultiplo x n    | mod x n==0 = x
                                | otherwise=0

sumarSoloMultiplos::(Integer,Integer,Integer)->Integer->Integer
sumarSoloMultiplos (x,y,z) n = multiploDevuelveMultiplo x n + multiploDevuelveMultiplo y n + multiploDevuelveMultiplo z n

--f
{-Especificacion:
problema posPrimerPar: (u:(NxNxN): Z {Asegura:{res=primer par de la terna}}
-}
posPrimerPar::(Integer,Integer,Integer)->Integer
posPrimerPar (x,y,z)    | even x =x
                        | even y =y
                        | even z =z
                        | otherwise=4

--g
{-Especificacion:
problema crearPar: (x:A , y:B):(AxB){Asegura:{res=(x,y)}}
-}
crearPar::tx->ty->(tx,ty)
crearPar x y = (x,y)

--h
{-Especificacion:
problema invertir: (v:AxB):(BxA) {Asegura: {res=(y,x)}}
-}
invertir:: (tx,ty)->(ty,tx)
invertir  (x,y) = (y,x)

--Ejercicio 5
{-Especificacion:
problema todosMenores: (t:ZxZxZ):Bool {
    requiere:{True}
    Asegura:{(res = true)<->((f(t0)>g(t0)) ∧ (f(t1)>g(t1)) ∧ (f(t2)>g(t2)))}
    }
    problema f:(n:Z):Z {requiere:{True} Asegura: {(n<=7->res=n^2) ^ (n>7->res=2n-1)}
    }
    problema g:(n:Z):Z {requiere:{True} Asegura:{Si n=2k con k entero distinto de cero -> res=n/2, si no res=3n+1}}
    --}

f5::Integer->Integer
f5 n    | n<=7=n^2
        |otherwise = 2*n-1
g5:: Integer->Integer
g5 n    | even n = div n 2
        | otherwise=3*n+1
todosMenores::(Integer,Integer,Integer)->Bool
todosMenores (x,y,z)    | f5 x>g x && f5 y>g5 y && f5 z>g5 z=True
                        | otherwise=False

--Ejercicio 6
{-Especificacion:
problema bisiesto (anio:Z): Bool {requiere: {True} Asegura:{res=false<->anio=/4*k v (anio=100*k ^ anio=/400*k}
}
-}

type Anio = Integer
type EsBisiesto = Bool
bisiesto:: Anio->EsBisiesto
bisiesto x  | mod x 4==0=True
            | otherwise=False

--Ejercicio 7

--a 
distancia :: Float->Float->Float
distancia x y = sqrt ((x-y)^2)

distanciaManhattan:: (Float,Float,Float)->(Float,Float,Float)->Float
distanciaManhattan (u0,u1,u2) (v0,v1,v2) = distancia u0 v0 + distancia u1 v1 + distancia u2 v2

--Ejercicio 8

sumaUltimosDosDigitos:: Integer->Integer
sumaUltimosDosDigitos x = mod (absoluto x) 10 + mod (div (absoluto x) 10) 10

comparar::Integer->Integer->Integer
comparar a b    | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
                | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
                | otherwise=0

--Ejercicio 9

{- a) f1 :: Float-> Float f1 n  | n == 0 = 1 
                                | otherwise = 0
f1:R->R/f(0)=1 ^ f(n)=0 con n≠0

b)f2 :: Float-> Float 
  f2 n  | n == 1 = 15 
        | n ==-1 =-15
-}