--Exercicio 2 Lab 3
lst1 = [x*2 | x <- [1..10], x*2 >= 12]
lst2 = [ x | x <- [50..100], mod x 7 == 3]
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
lst4=[(x,y)| x <- [1..4], y <- [x..5]]
npares xs = length [x | x<-xs, even x]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

--Exercicio 3 Lab 3
listaQuad = [x^2|x<-[1..100]]

--Exercicio 4 Lab 3
listaQuadPares = [x^2|x<-[1..100], even x]

--Exercicio 5 Lab 3
quadrados::Int->Int->[Int]
quadrados x y = [xs^2|xs<-[x..y]]

--Exercicio 6 Lab 3
seleciona_ímpares::[Int]->[Int]
seleciona_ímpares x = [y|y<-x, odd y]

--Exercicio 7 Lab 3
tabuada::Int->[Int]
tabuada x = [y|y<-[x*1,x*2..x*10]]

--Exercicio 8 Lab 3
bissextos::[Int]->[Int]
bissextos x = [y|y<-x, mod y 4 == 0 && mod y 100 /= 0 || mod y 400 == 0]

--Exercicio 9 Lab 3
sublistas::[[a]]->[a]
sublistas x = [y|y<-concat x]

--Exercicio 10 Lab 3
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =  [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),	("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

bissexto::Int->Bool
bissexto x = if (mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) then True else False

valida::Data->Bool
valida (d,m,a)
	| d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||
	 m == 8 || m == 10 || m == 12) = True
	| d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
	| d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
	| d <= 28 && d >= 1 && m == 2 = True
	| otherwise	= False

checaEmprestimo::Emprestimo->Data->Bool
checaEmprestimo (_,_,(d1,m1,a1),(d2,m2,a2),_) (d3,m3,a3)
	|(d3 > d1) && (m3 == m1) && (a3 == a1) && (valida (d3,m3,a3) == True) && 
	(valida (d1,m1,a1) == True) && (d2 > d3) && (m2 == m3) && (a2 == a3) && (valida (d3,m3,a3) == True) && 
	(valida (d2,m2,a2) == True) = True
	| (m3 > m1) && (a3 == a1) && (valida (d1,m1,a1) == True) && 
	(valida (d3,m3,a3) == True) && (m2 > m3) && (a2 == a3) && (valida (d2,m2,a2) == True) && 
	(valida (d3,m3,a3) == True) = True
	| (a3 > a1) && (valida (d1,m1,a1) == True) && (valida (d3,m3,a3) == True) &&
	(a3 > a2) && (valida (d2,m2,a2) == True) = True
	| otherwise = False

atrasados::Emprestimos->Data->[Emprestimo]
atrasados x y = [z|z<-x, checaEmprestimo z y == False]