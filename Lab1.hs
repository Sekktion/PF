--Vinícius Goulart de Farias Meres
--Matricula: 11811BCC009

dobro::Int->Int --Ex 2
dobro x = x*2

quad::Int->Int --Ex 3
quad x = dobro (dobro x) 

pitagoras::Double->Double->Double --Ex 4
pitagoras c1 c2 = sqrt ((c1^2) + (c2^2))

distPontos::Double->Double->Double->Double->Double --Ex 5
distPontos x1 y1 x2 y2 = sqrt (((x2-x1)^2)+((y2-y1)^2))

par::Int->Bool --Ex 6a
par x = if mod x 2 == 0 then True else False

impar::Int->Bool --Ex 6b
impar x = not (par x)

convTemp::Double->Double --Ex 7
convTemp x = (x - 32)/1.8

maiorEntreDois::Int->Int->Int --Ex 8
maiorEntreDois x y = if x > y then x else y

maiorEntreTres::Int->Int->Int->Int --Ex 9
maiorEntreTres x y z = if x>y && x>z then x else if y>x && y>z then y else z

maiorQueZero::Int->Int --Ex 10
maiorQueZero x = if x>0 then 1 else if x<0 then (-1) else 0

ehLetra::Char->Bool --Ex 11
ehLetra x = if (x >= 'a' && x <= 'z') || (x >= 'A' || x <= 'Z') then True else False

ladosTriangRet::Double->Double->Double->Bool --Ex 12
ladosTriangRet cat1 cat2 hip = if sqrt ((cat1^2) + (cat2^2)) == hip then True else False
