--Vinícius Goulart de Farias Meres
--11811BCC009

--Exercicio 1 A
ex1a1::Bool->Bool->Bool
ex1a1 True True = True
ex1a1 True False = True
ex1a1 False True = True
ex1a1 False False = False

ex1a2::Bool->Bool->Bool
ex1a2 False False = False
ex1a2 True _ = True

ex1a3::Bool->Bool->Bool
ex1a3 False b = b
ex1a3 True _ = True

--Exercicio 1 B
ex1b1::Bool->Bool->Bool
ex1b1 a b =
  if (a == False && b == False)
    then False
    else True

ex1b2::Bool->Bool->Bool
ex1b2 a b =
  if (a /= b)
    then True
    else
      if (a == b && b == False)
        then False
        else True

--Exercicio 2
quadrado::Float->Float
quadrado x = x * x

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (x1, y1) (x2, y2) = sqrt (quadrado (x2 - x1) + quadrado (y2 - y1))

--Exercicio 4
fatorial::Int->Int
fatorial 0 = 1
fatorial n = n * fatorial (n -1)

fatorial2::Int->Int
fatorial2 x = if(x==0) then 1
            else (x*fatorial2 (x-1))

--Exercicio 5
fibo::Int->Int
fibo n
 | n == 0 = 1
 | n == 1 = 1
 | otherwise = fibo (n -2) + fibo (n -1)

--Exercicio 6
n_tri::Int->Int
n_tri n
 | n == 0 = 0
 | n == 1 = 1
 | n == 2 = 3
 | n == 3 = 6
 | otherwise = n_tri (n -3) + n_tri (n -2) + n_tri (n -1)

--Exercicio 7
passo::(Int, Int)->(Int, Int)
passo (x, y) = (y, x + y)

auxFibo::Int->(Int,Int)
auxFibo 1 = (1, 1)
auxFibo n = passo (auxFibo (n-1))

fibo2::Int->Int
fibo2 n = do
    let (x, y) = auxFibo n
    x

--Exercicio 8
potencia2::Int->Int
potencia2 expo
 | expo == 0 = 1
 | otherwise = (2 * potencia2 (expo -1))

--Exercicio 9 A
prodIntervalo::Int->Int->Int
prodIntervalo m n
 | m == n = n
 | otherwise = n * prodIntervalo m (n -1)

--Exercicio 9 B
fiboProdIntervalo::Int->Int
fiboProdIntervalo  n
 | 1 == n = n
 | otherwise = n * fiboProdIntervalo  (n -1)

--Exercicio 11
resto_div::Int->Int->Int
resto_div dividendo divisor =
  if dividendo == 0 || divisor == 1
    then 0
    else
      if dividendo < divisor
        then dividendo
        else resto_div (dividendo - divisor) divisor

div_inteira::Int->Int->Int
div_inteira dividendo divisor =
  if (dividendo < divisor)
    then 0
    else (div_inteira (dividendo - divisor) divisor) + 1

--Exercicio 12
--Com guardas:
mdcGuardas::(Int, Int)->Int
mdcGuardas (m, n)
 | n == 0 = m
 | otherwise = mdcGuardas (n, (mod m n))

--Casamento de Padrões
mdc::(Int, Int)->Int
mdc (m, 0) = m
mdc (m, n) = mdc (n, (mod m n))

--Exercicio 13
--Com Guardas
binomialGuardas::(Int, Int)->Int
binomialGuardas (n, k)
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomialGuardas (n -1, k) + binomialGuardas (n -1, k -1)

--Casamento de Padrões
binomial::(Int, Int)->Int
binomial (n, 0) = 1
binomial (n, k) =
  if (k == n)
    then 1
    else binomial (n -1, k) + binomial (n -1, k -1)

--Exercicio 15 A
ex15a::Int->Int->[Int]
ex15a a b 
 | a==b = a:[]
 | a>b=[]
 | otherwise=[a..b]

--Exercicio 15 B
ex15b::Int->Int->[Int]
ex15b a b
 | a == b || a > b = []
 | otherwise = [a+1..b-1]

 --reverse [1..5]
 --['a','c'..'e']
 --[1,4..16]
 --zip [1,-2..(-11)] [1,5..17]
 --