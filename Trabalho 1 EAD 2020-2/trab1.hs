--Vinícius Goulart de Farias Meres - 11811BCC009
-- e
-- Gabriel Zeitoum dos Santos - 11811BCC018
-- Trabalho 1 Programação Funcional - Gina Oliveira

--Exercicio 1
triangulo::Int->Int->Int->String
triangulo a b c
 | a + b + c > 180 || a + b + c < 180 = "nao_triangulo"
 | a == b && b == c && c == a = "equilatero"
 | a == 90 || b == 90 || c == 90 = "retangulo"
 | a > 90 || b > 90 || c > 90 = "obtuso"
 | otherwise = "simples"
 
--Exercicio 2
equacao::Double->Double->Double->(Double,Double)
equacao a b c 
 | a /= 0 = ((-b+sqrt(b*b-4*a*c))/2*a,(-b-sqrt(b*b-4*a*c))/2*a)
 | otherwise = ((-c)/b, a)

--Exercicio 3
type Data = (Int,Int,Int)

bissexto::Int->Bool
bissexto x = if (mod x 4 == 0 && mod x 100 /= 0 || mod x 400 == 0) then True else False

valida::Data->Bool
valida (d,m,a)
 | d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 ||
 m == 8 || m == 10 || m == 12) = True
 | d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11) = True
 | d <= 29 && d >= 1 && m == 2 && (bissexto a == True) = True
 | d <= 28 && d >= 1 && m == 2 = True
 | otherwise = False

precoPassagem::Data->Data->Float->Float
precoPassagem (d1,m1,a1) (d2,m2,a2) preco 
 | (valida (d1,m1,a1)) == False || (valida (d2,m2,a2)) == False = 0
 | a2 - a1 < 2 = preco*0.15
 | a2 - a1 == 2 && m1 > m2 = preco*0.15
 | a2 - a1 == 2 && m1 == m2 && d1 > d2 = preco*0.15
 | a2 - a1 < 10 = preco*0.40
 | a2 - a1 == 10 && m1 > m2 = preco*0.40
 | a2 - a1 == 10 && m1 == m2 && d1 > d2 = preco*0.40
 | a2 - a1 > 70 = preco/2
 | a2 - a1 == 70 && m1 < m2 = preco/2
 | a2 - a1 == 70 && m1 == m2 && d1 <= d2 = preco/2
 | otherwise = preco

--Exercicio 4
-- Print do resultado das listas: https://prnt.sc/u964yj

--Exercicio 5
contaNegM2::[Int]->Int
contaNegM2 x = length [y | y <- x, y<0, mod y 2 == 0]

listaNegM2::[Int]->[Int]
listaNegM2 x = [y | y <- x, y<0, mod y 2 == 0]

--Exercicio 6
distancias :: [(Float,Float)] -> [Float]
distancias [] = []
distancias ((x,y):xys) = [r | r <-(sqrt (x^2 + y^2)):(distancias xys)]

--Exercicio 7
fatores::Int->[Int]
fatores n =  [x | x <- [1..n], mod n x == 0]
primos::Int->Int->[Int]
primos x y = [r | r<-[x..y], length (fatores r) == 2]

--Exercicio 8
mdc::Int->Int->Int
mdc a b
 |b == 0 = a
 |otherwise = mdc b (mod a b)
 
mmc2::Int->Int->Int
mmc2 a b
 |a == b = a
 | otherwise = div (a*b) (mdc a b)
 
mmc::Int->Int->Int->Int
mmc a b c = mmc2 a (mmc2 b c)

--Exercicio 9
exercicio9::Float->Int->Float
exercicio9 x n
 | x == 0 || n == 0 = 0
 | mod n 2 == 0 = x/fromIntegral(n) + exercicio9 x (n-1)
 | mod n 2 /= 0 = fromIntegral(n)/x + exercicio9 x (n-1)

--Exercicio 10
fizzbuzz2::Int->[String]
fizzbuzz2 n
 | n == 0 = []
 | mod n 3 == 0 && mod n 5 == 0 = "FizzBuzz":fizzbuzz2 (n-1)
 | mod n 3 == 0 = "Fizz":fizzbuzz2 (n-1)
 | mod n 5 == 0 = "Buzz":fizzbuzz2 (n-1)
 | otherwise = "No":fizzbuzz2 (n-1)

fizzbuzz::Int->[String]
fizzbuzz x = reverse (fizzbuzz2 x)

--Exercicio 11
conta_ocorrencias::Int->Int->[Int]->(Int,Int)
conta_ocorrencias x y lst = (length [r1 | r1 <- lst, r1 == x], length [r2 | r2 <- lst, r2 == y])
 
--Exercicio 12
conta_zeite::Int->[Int]->Int
conta_zeite x lst
 | length lst == 0 = 0
 | head lst == x = 1 + conta_zeite x (tail lst)
 | otherwise = conta_zeite x (tail lst)

unica_ocorrencia::Int->[Int]->Bool
unica_ocorrencia x lst 
 | (conta_zeite x lst) > 1 || (conta_zeite x lst) < 1 = False
 | otherwise = True

--Exercicio 13
intercala::[Int]->[Int]->[Int]
intercala x y
 | x == [] && y == [] = []
 | x == [] = y
 | y == [] = x
 | otherwise = [head x]++[head y]++(intercala (tail x) (tail y))
 
--Exercicio 14
type Contato = (String,String,String,String)

contatos::[Contato]
contatos = [("Vinicius","Rua Atilio Valentini","16981904490","viniciusmeres1@gmail.com"),
 ("Gabriel","Rua Atilio Valentini","17996221115","gabrielzeitoum@hotmail.com"),
 ("Dirlan","Rua Dirlan","24-69","dirlan@gmail.com")]

encontraContatoAux::[Contato]->String->String
encontraContatoAux [] email = "Email nao encontrado"
encontraContatoAux ((x,y,z,w):xs) email
 | w == email = x
 | otherwise = encontraContatoAux xs email
 
encontraContato::String->String
encontraContato email = encontraContatoAux contatos email

--Exercicio 15	
type Pessoa = (String,  Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ 
 ("João", 1.85, 26, 'C'),
 ("Maria", 1.55, 62, 'S'),
 ("Jose", 1.78, 42, 'C'),
 ("Paulo", 1.93, 25, 'S'),
 ("Clara", 1.70, 33, 'C'),
 ("Bob", 1.45, 21, 'C'),
 ("Rosana", 1.58, 39, 'S'),
 ("Daniel", 1.74, 72, 'S'),
 ("Jocileide", 1.69, 18, 'S') ]

-- A
media lista = (soma_altura lista)/ fromIntegral (length lista) :: Float
soma_altura::[Pessoa]->Float
soma_altura [] = 0.0
soma_altura ((nome,  altura, idade, estado):xs) = altura + (soma_altura xs)

-- B
idade_mais_nova::[Pessoa]->Int->Int
idade_mais_nova [] y = y
idade_mais_nova ((nome, altura, idade, estado):xs) y = if idade<y then  idade_mais_nova xs idade
                                                                  else  idade_mais_nova xs y 

menor_idade::[Pessoa]->Int
menor_idade lista = (idade_mais_nova lista 999)

-- C
idade_mais_velha::[Pessoa]->Int->Int
idade_mais_velha [] y = y
idade_mais_velha ((nome, altura, idade, estado):xs) y = if idade>y then idade_mais_velha xs idade   
                                                                   else idade_mais_velha xs y

maior_idade::[Pessoa]->Int
maior_idade lista = (idade_mais_velha lista 0)

encontraMaisVelhaAux::[Pessoa]->Int->(String,Char)
encontraMaisVelhaAux [] i = ("Vazio", 'V')
encontraMaisVelhaAux ((x,y,z,w):xs) i
 | z == i = (x,w)
 | otherwise = encontraMaisVelhaAux xs i

encontraMaisVelha::[Pessoa]->(String,Char)
encontraMaisVelha lista = encontraMaisVelhaAux lista (maior_idade pessoas)

-- D
encontra50Anos::[Pessoa]->[Pessoa]->[Pessoa]
encontra50Anos [] _ = []
encontra50Anos ((nome, altura, idade, estado):xs) y = if idade >= 50 then 
    encontra50Anos xs y++[(nome, altura, idade, estado)] else 
        encontra50Anos xs y

retornaDados::[Pessoa]->[Pessoa]
retornaDados lista = encontra50Anos lista []

-- E
encontraPessoasCasadas::[Pessoa]->Int->[Pessoa]->[Pessoa]
encontraPessoasCasadas [] _ _ = []
encontraPessoasCasadas ((nome, altura, idade, estado):xs) i y = if ((idade >= i) && (estado == 'C')) then 
    encontraPessoasCasadas xs i y++[(nome, altura, idade, estado)] else 
        encontraPessoasCasadas xs i y

pessoasCasadas::[Pessoa]->Int->[Pessoa]
pessoasCasadas lista i = encontraPessoasCasadas lista i []

--Exercicio 16
insere_ord::Ord a=>a->[a]->[a]
insere_ord x [] = [x]
insere_ord x (y:ys)
 | x <= y = (x:y:ys)
 | otherwise = y: (insere_ord x ys)

--Exercicio 17
reverte::[a]->[a]
reverte lst
 | null lst == True = []
 | otherwise = (reverte (tail lst))++[(head lst)]

--Exercicio 18
elemc :: Eq a => ([a],a) -> Bool
elemc ([], _) = False
elemc ((x:xs), y) = if x == y then True else elemc (xs, y)

outra_lista::Eq a => [a]->[a]->[a]
outra_lista [] y = y
outra_lista (x:xs) y = if elemc (y, x) == False then outra_lista xs (x:y)
 else outra_lista xs y 

sem_repetidos::Eq a => [a]->[a]
sem_repetidos lst
 | null lst == True = []
 | otherwise = reverse (outra_lista lst [])

--Exercicio 19
disponiveis::[Int]
disponiveis = [1,2,5,10,20,50,100]

notasTroco::Int->[[Int]]
notasTroco 0 = [[]]
notasTroco valor = [v:vs | v <- disponiveis, valor >= v,
 vs <- notasTroco (valor-v)]

--Exercicio 20
-- criaLista::Int->[Int]
-- criaLista n = [1..n]

-- verificaLista:

-- atribuiRainhas

-- nRainhas::Int->[Int]
-- nRainhas n = [r1 | <- [1..n]]
