import Db 
-- 0035411     Lucas Mateus Fernandes
-- 0020640     Pedro Daniel Camargos Soares


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
    | qtd(maxP) >= qtd(h) = maxV t maxP
    | otherwise = (maxP)

maisSorteado = maisSorteadoAUX db



main = do
     
	putStrLn("Escolha uma opcao:")
	putStrLn("1- O numero X ja foi sorteado alguma vez?")
	putStrLn("2- O jogo (X1,X2,X3,X4,X5,X6) ja foi contemplado alguma vez?")
	putStrLn("3- Um numero X foi sorteado quantas vezes?")
	putStrLn("4- Qual o numero foi mais sorteado?")
	putStrLn("0- Sair")


	opcao <- getLine
	case opcao of
		"1" -> opc1
		"2" -> opc2
		"3" -> opc3
		"4" -> opc4
		"0" -> putStrLn("Falouuuu")
        

   

opc1 = do
    putStrLn "Digite o numero que deseja verificar"
    numero <- readLn
    if (sorteado numero)
        then putStrLn ("O numero "++(show numero)++" ja foi sorteado.")
        else putStrLn ("O numero "++(show numero)++" nunca foi sorteado.")
    
    putStrLn ("\nDigite qualquer tecla para continuar")
    getChar
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    main

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

    if (jogoSorteado numero1 numero2 numero3 numero4 numero5 numero6)
        then putStrLn ("O jogo "++(show numero1)++" "++(show numero2)++" "++(show numero3)++" "++(show numero4)++" "++(show numero5)++" "++(show numero6)++" ja foi sorteado pelo menos uma vez.")
        else putStrLn ("O jogo "++(show numero1)++" "++(show numero2)++" "++(show numero3)++" "++(show numero4)++" "++(show numero5)++" "++(show numero6)++" nunca foi sorteado.")

    putStrLn ("\nDigite qualquer tecla para continuar")
    getChar
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    main

opc3 = do
    putStrLn "Digite o numero a ser pesquisado"
    numero <- readLn

    putStrLn ("O numero "++(show numero)++" foi sorteado "++( show (quantSorteado numero))++" vezes")
    putStrLn ("\nDigite qualquer tecla para continuar")
    getChar
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    main

    

opc4 = do
    putStrLn("O numero "++(show maisSorteado)++" foi o mais sorteado")
    putStrLn ("\nDigite qualquer tecla para continuar")
    getChar
    putStrLn "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    main
    

