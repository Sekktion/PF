-- Vinícius Goulart de Farias Meres - 11811BCC009 
-- Programação Funcional - Gina Oliveira

-- Lista 4

-- Exercicio 1
-- Print na pasta deste arquivo

-- Exercicio 2
quadrados::Int->Int->[Int]
quadrados x y = [r*r | r <- [x..y]]

-- Exercicio 3
seleciona_impares::[Int]->[Int]
seleciona_impares lst = [r | r <- lst, mod r 2 == 1]

-- Exercicio 4
tabuada::Int->[Int]
tabuada x = [x*2,x*3..x*10]

-- Exercicio 5
bissexto::Int->Bool
bissexto x = if (mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) then True else False

bissextos::[Int]->[Int]
bissextos lst = [r | r <- lst, bissexto r == True]

-- Exercicio 6
sublistas::[[a]]->[a]
sublistas [] = []
sublistas (x:xs) = x++[r | r <- sublistas xs]

-- Exercicio 7
type Data = (Int,Int,Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

valida::Data->Bool
valida (d,m,a)
 | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||
 m == 8 || m == 10 || m == 12) = True
 | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
 | d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
 | d <= 28 && d >= 1 && m == 2 = True
 | otherwise = False

precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2)
 | (d2 > d1) && (m2 == m1) && (a2 == a1) && (valida (d1,m1,a1) == True) && 
 (valida (d2,m2,a2) == True) = True
 | (m2 > m1) && (a2 == a1) && (valida (d1,m1,a1) == True) && 
 (valida (d2,m2,a2) == True) = True
 | (a2 > a1) && (valida (d1,m1,a1) == True) && (valida (d2,m2,a2) 
 == True) = True
 | otherwise = False

pegaData::Emprestimo->Data
pegaData (_,_,_,(d,m,a),_) = (d,m,a)

atrasados::Emprestimos->Data->Emprestimos
atrasados bdEmprestimo (d,m,a) = [ r | r <- bdEmprestimo, (precede (pegaData r) (d,m,a)) == True]

-- Exercicio 8
pares::[Int]->[Int]
pares xs = [x | x<-xs, even x]

npares::[Int]->Int
npares x = length(pares x)


--Exercicio 9
produtorio::[Int]->Int
produtorio []= 0
produtorio [x]= x

produtorio (x:xs) = x*(produtorio xs)

--Exercicio 10
comprime::[[a]]->[a]
comprime [] = []
comprime (x:xs) = x ++ comprime xs

--Exercicio 11
contador::[a]->Int->Int
contador [] y = y
contador (x:xs) y = contador xs y+1

tamanho::[a]->Int
tamanho x = contador x 0

--Exercicio 12
uniaoNRec::[Int]->[Int]->[Int]
uniaoNRec x y = x++[r | r <- y, (elem r x) == False]

--Exercicio 13
uniaoRec2::Eq a =>[a]->[a]->[a]
uniaoRec2 [] y = y
uniaoRec2 x [] = x
uniaoRec2 x (y:ys) = if (elem y x) == True then
    uniaoRec2 x ys else
        uniaoRec2 (x++[y]) ys