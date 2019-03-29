conversao::Float->(Float,Float,Float)  --Exercicio 2
conversao x = (x,x*3.96,x*4.45)

bissexto::Int->Bool  --Exercicio 3
bissexto x = if (mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) then True else False

type Data = (Int,Int,Int) --Exercicio 4
bissexto2::Data->Bool
bissexto2 (d,m,a)   
	| d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||
	 m == 8 || m == 10 || m == 12) && (bissexto a == True) = True
	| d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11)
	 && (bissexto a == True) = True
	| d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
	| otherwise	= False
	
valida::Data->Bool --Exercicio 5
valida (d,m,a)
	| d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||
	 m == 8 || m == 10 || m == 12) = True
	| d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
	| d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
	| d <= 28 && d >= 1 && m == 2 = True
	| otherwise	= False

precede::Data->Data->Bool --Exercicio 6
precede (d1,m1,a1) (d2,m2,a2)
	| (d2 > d1) && (m2 == m1) && (a2 == a1) && (valida (d1,m1,a1) == True) && 
	(valida (d2,m2,a2) == True) = True
	| (m2 > m1) && (a2 == a1) && (valida (d1,m1,a1) == True) && 
	(valida (d2,m2,a2) == True) = True
	| (a2 > a1) && (valida (d1,m1,a1) == True) && (valida (d2,m2,a2) 
	 == True) = True
	| otherwise = False
	
type Livro = (String,String,String,String,Int) --Exercicio 7
type Aluno = (String, String, String, Int)
type Emprestimo = (String, String, Data, Data, String)

e1::Emprestimo  --Exercicio 8
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

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
