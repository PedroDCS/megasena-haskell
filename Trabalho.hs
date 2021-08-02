db = [(2375,02,06,44,46,53,58),(2374,12,13,25,37,39,41)]

numInGame n (j,n1,n2,n3,n4,n5,n6) 
    | n == n1 = True 
    | n == n2 = True  
    | n == n3 = True 
    | n == n4 = True 
    | n == n5 = True 
    | n == n6 = True
    | otherwise = False 

sorteadoX num (j:other)
    | numInGame num j = True
    | (null other) = False
    | sorteadoX num other == True = True
    | otherwise = False

sorteado num = sorteadoX num db

--jogoEq n1 n2 n3 n4 n5 n6 (j, s1, s2, s3, s4, s5, s6) = s1 == n1 && s2 == n2 && s3 == n3 && s4 == n4 && s5 == n5 && s6 == n6

jogoEq n1 n2 n3 n4 n5 n6 jogo =
    numInGame n1 jogo && numInGame n2 jogo && numInGame n3 jogo && numInGame n4 jogo && numInGame n5 jogo && numInGame n6 jogo && n1 /= n2 && n1 /= n3 && n1 /= n4 && n1 /= n5 && n1 /= n6 && n2 /= n3 && n2 /= n4 && n2 /= n5 && n2 /= n6 && n3 /= n4 && n3 /= n5 && n3 /= n6 && n4 /= n5 && n4 /= n6 && n5 /= n6              

jogoSorteadoX n1 n2 n3 n4 n5 n6 (j:other)
    | jogoEq n1 n2 n3 n4 n5 n6 j = True
    | (null other) = False
    | jogoSorteadoX n1 n2 n3 n4 n5 n6 other == True = True
    | otherwise = False



jogoSorteado n1 n2 n3 n4 n5 n6 = jogoSorteadoX n1 n2 n3 n4 n5 n6 db



qtdX x db = qtdXAux x 0 db 
qtdXAux x t (j:other) 
    | ((null other) && (numInGame x j)) = (t+1)
    | ((null other) && (not(numInGame x j))) = t
    | (numInGame x j ) = qtdXAux x (t+1) other 
    | (not(numInGame x j)) = qtdXAux x t other 
    | otherwise = -1

quantSorteado num = qtdX num db

-- para cada numero 'x', no intervalo[1-60] verifica a sua quantidade,
-- dado um vetor de quantidade de ocorrencias retorna o numero que mais foi sorteado
maisSorteadoAUX db = num(maior [(x,(qtdX x db))|x<-[1..60]]) 

num (i,qtdi) = i -- retorna o numero do par ordenado
qtd (i,qtdi) = qtdi -- retorna a quantidade que o numero 'i' apareceu

-- retorna o maior numero da lista
maior (h:t) 
    | t == [] = h
    | otherwise = maxV t h

maxV (h:t) maxP -- salva a tupla ordenada referente ao numero que mais aparece
    | t == [] && qtd(maxP) >= qtd(h) = maxP
    | t == [] && qtd(h) > qtd(maxP) = h
    | qtd(h) > qtd(maxP) = maxV t h
    | qtd(maxP) > qtd(h) = maxV t maxP
    | otherwise = (maxP)

maisSorteado = maisSorteadoAUX db



menu = do
	putStrLn("Escolha uma opcao:")
	putStrLn("1- O numero X ja foi sorteado alguma vez?")
	putStrLn("2- O jogo (X1,X2,X3,X4,X5,X6) ja foi contemplado alguma vez?")
	putStrLn("3- Um numero X foi sorteado quantas vezes?")
	putStrLn("4- Qual o numero foi mais sorteado?")
	opcao <- getLine
	case opcao of
		"1" -> opc1
		"2" -> opc2
		"3" -> opc3
		"4" -> print(maisSorteado)

numb x
    | x=="1" = 1
    | x=="2" = 2
    | x=="3" = 3
    | x=="4" = 4
    | x=="5" = 5
    | x=="6" = 6
    | x=="7" = 7
    | x=="8" = 8
    | x=="9" = 9
    | x=="10" = 10
    | x=="11" = 11
    | x=="12" = 12
    | x=="13" = 13
    | x=="14" = 14
    | x=="15" = 15
    | x=="16" = 16
    | x=="17" = 17
    | x=="18" = 18
    | x=="19" = 19
    | x=="20" = 20
    | x=="21" = 21
    | x=="22" = 22
    | x=="23" = 23
    | x=="24" = 24
    | x=="25" = 25
    | x=="26" = 26
    | x=="27" = 27
    | x=="28" = 28
    | x=="29" = 29
    | x=="30" = 30
    | x=="31" = 31
    | x=="32" = 32
    | x=="33" = 33
    | x=="34" = 34
    | x=="35" = 35
    | x=="36" = 36
    | x=="37" = 37
    | x=="38" = 38
    | x=="39" = 39
    | x=="40" = 40
    | x=="41" = 41
    | x=="42" = 42
    | x=="43" = 43
    | x=="44" = 44
    | x=="45" = 45
    | x=="46" = 46
    | x=="47" = 47
    | x=="48" = 48
    | x=="49" = 49
    | x=="50" = 50
    | x=="51" = 51
    | x=="52" = 52
    | x=="53" = 53
    | x=="54" = 54
    | x=="55" = 55
    | x=="56" = 56
    | x=="57" = 57
    | x=="58" = 58
    | x=="59" = 59
    | x=="60" = 60
    | otherwise = 0
    

opc1 = do
    putStrLn "Digite o numero que deseja verificar"
    numero <- readLn
    print (sorteado numero)

opc2 = do
    putStrLn "Digite os Numeros na ordem crescente"
    putStrLn "Digite o primeiro numero do jogo"
    numero1 <- readLn

    putStrLn "Digite o segundo numero do jogo"
    numero2 <- readLn

    putStrLn "Digite o terceiro numero do jogo"
    numero3 <- readLn

    putStrLn "Digite o quarto numero do jogo"
    numero4 <- readLn

    putStrLn "Digite o quinto numero do jogo"
    numero5 <- readLn

    putStrLn "Digite o sexto numero do jogo"
    numero6 <- readLn

    print(jogoSorteado numero1 numero2 numero3 numero4 numero5 numero6)

opc3 = do
    putStrLn "Digite o numero a ser pesquisado"
    numero <- readLn

    print ("Foi sorteado "++( show (quantSorteado numero))++" vezes")



