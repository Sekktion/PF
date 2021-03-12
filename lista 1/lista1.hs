--Vinícius Goulart de Farias Meres
--Matricula: 11811BCC009

--Ex 2
dobro::Int->Int
dobro x = x*2

--Ex 3
quad::Int->Int
quad x = dobro (dobro x) 

--Ex 4
pitagoras::Double->Double->Double
pitagoras c1 c2 = sqrt ((c1^2) + (c2^2))

--Ex 5
distPontos::Double->Double->Double->Double->Double
distPontos x1 y1 x2 y2 = sqrt (((x2-x1)^2)+((y2-y1)^2))