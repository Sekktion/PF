-- Ex 1
-- A
bissexto :: Int -> Bool
bissexto ano = exp1 || (exp2 && exp3)
  where
    exp1 = (mod ano 400 == 0)
    exp2 = (mod ano 4 == 0)
    exp3 = (mod ano 100 /= 0)

type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (dia, mes, ano) = exp1 || exp2 || exp3 || exp4
  where
    exp1 = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
    exp2 = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
    exp3 = dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano)
    exp4 = dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano)

-- B
bissextos :: [Int] -> [Int]
bissextos lista = exp1
  where
    exp1 = [x | x <- lista, bissexto x]

-- C
type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procede :: Data -> Data -> Bool
procede (dia, mes, ano) (dia2, mes2, ano2) = not (exp1 || exp2 || exp3 || exp4)
  where
    exp1 = not (valida (dia, mes, ano)) || not (valida (dia2, mes2, ano2))
    exp2 = ano > ano2
    exp3 = ano == ano2 && mes > mes2
    exp4 = ano == ano2 && mes == mes && dia > dia2

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) = exp1
  where
    exp1 = procede dataAtual dataDevo

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = exp1
  where
    exp1 = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

-- D
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = exp1
  where
    exp1 = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n = exp1
  where
    exp1 = passo (fibo2 (n -1))

-- E
prodIntervalo :: Int -> Int -> Int
prodIntervalo x y = if (x >= y) then exp1 else exp2
  where
    exp1 = y
    exp2 = (x * (prodIntervalo (x + 1) y))

fatorial :: Int -> Int
fatorial n = exp1
  where
    exp1 = prodIntervalo 1 n

-- Ex 2
-- A
bissexto2 :: Int -> Bool
bissexto2 ano =
  let exp1 = (mod ano 400 == 0)
      exp2 = (mod ano 4 == 0)
      exp3 = (mod ano 100 /= 0)
   in exp1 || (exp2 && exp3)

type Data2 = (Int, Int, Int)

valida2 :: Data2 -> Bool
valida2 (dia, mes, ano) =
  let exp1 = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
      exp2 = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
      exp3 = dia >= 1 && dia <= 28 && mes == 2 && not (bissexto2 ano)
      exp4 = dia >= 1 && dia <= 29 && mes == 2 && (bissexto2 ano)
   in exp1 || exp2 || exp3 || exp4

-- B
bissexto3 :: [Int] -> [Int]
bissexto3 lista =
  let exp1 = [x | x <- lista, bissexto2 x]
   in exp1

-- C
type Emprestimo2 = (String, String, Data2, Data2, String)

type Emprestimos2 = [Emprestimo2]

bdEmprestimo2 :: Emprestimos2
bdEmprestimo2 =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procede2 :: Data2 -> Data2 -> Bool
procede2 (dia, mes, ano) (dia2, mes2, ano2) =
  let exp1 = not (valida2 (dia, mes, ano)) || not (valida2 (dia2, mes2, ano2))
      exp2 = ano > ano2
      exp3 = ano == ano2 && mes > mes2
      exp4 = ano == ano2 && mes == mes && dia > dia2
   in not (exp1 || exp2 || exp3 || exp4)

emprestimoEmDia2 :: Data2 -> Emprestimo2 -> Bool
emprestimoEmDia2 dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) =
  let exp1 = procede2 dataAtual dataDevo
   in exp1

atrasados2 :: Emprestimos2 -> Data2 -> Emprestimos2
atrasados2 listaEmprestimos dataAtual =
  let exp1 = [x | x <- listaEmprestimos, not (emprestimoEmDia2 dataAtual x)]
   in exp1

-- D
passo2 :: (Int, Int) -> (Int, Int)
passo2 (x, y) =
  let exp1 = (y, x + y)
   in exp1

fibo2comLet :: Int -> (Int, Int)
fibo2comLet 0 = (0, 1)
fibo2comLet n =
  let exp1 = passo2 (fibo2comLet (n -1))
   in exp1

-- E
prodIntervalo2 :: Int -> Int -> Int
prodIntervalo2 m n =
  let exp1 =
        if (m >= n)
          then n
          else (m * (prodIntervalo (m + 1) n))
   in exp1

fatorial2 :: Int -> Int
fatorial2 n =
  let exp1 = prodIntervalo 1 n
   in exp1

-- Ex 3 
    -- A

    -- (\x. 2 * x + 1) 3
    -- 2*3 + 1
    -- 6 + 1
    -- 7


    -- B

    -- (\ xy. x-y) 5 7
    -- 5 - 7
    -- -2


    -- C

    -- (\ yx. x-y) 5 7
    -- 7 - 5
    -- 2


    -- D

    -- (\ xy. x-y) (\z. z/2)
    -- (\y.  (z/2) - y)


    -- E

    -- (\ xy. x-y) ((\z. z/2) 6 ) 1
    -- (\ xy. x-y) (6/2) 1
    -- (\ xy. x-y) 3 1
    -- 3 - 1
    -- 2


    -- F

    -- (\ x. \ y. - x y) 9 4
    -- (\ x. - x 4) 9 
    -- ( - 9 4) 
    -- 5


    -- G

    -- (\ x. xx) (\ y. y)
    -- (\ y. yy)


-- Ex 4 https://prnt.sc/un9f7v    <-- Print do console

-- Ex 5 https://prnt.sc/un9gaa    <-- Print do console

lambda1::Int
lambda1 = (\x -> \y -> y) ((\z -> z) (\z -> z)) (\w -> w) 5

lambda2::Int
lambda2 = ((\f -> (\x -> f (f x))) (\y -> (y * y))) 3

lambda3::Int
lambda3 = ((\f -> (\x -> f (f x))) (\y -> (y + y))) 5

lambda4::Int
lambda4 = ((\x -> (\y -> x + y) 5) ((\y -> y -3) 7))

lambda5::Int
lambda5 = (((\f -> (\x -> f (f (f x)))) (\y -> (y * y))) 2)

lambda6::Int
lambda6 = (\x -> \y -> x + ((\x -> x - 3) y)) 5 6