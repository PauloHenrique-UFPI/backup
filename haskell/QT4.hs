proxima([]) = []
proxima(h:t)
    | h == ' ' = t
    | otherwise = proxima(t)

separa([]) = []    
separa(h:t)
    | h == ' ' = []
    | otherwise = h : separa(t)

minhaLista([]) = []
minhaLista(lista) = separa(lista) : minhaLista(proxima(lista))


repete(letra, []) = False
repete(letra, h : t) 
    | letra /= h = repete(letra, t)
    | otherwise = True

conta([], lista) = 0
conta(h:t, lista)
    | repete(h, lista) == False = 1 + conta(t, h:lista)
    | otherwise = 0 + conta(t, lista)

contaLista([]) = putStr "\n"
contaLista(h:t) = do
    putStr ("Item: " ++ h ++ " Tamanho: ")
    print(conta(h, []))
    contaLista(t)

vogaisMini = ['a','e','i','o','u']
vogaisMai = ['A','E','I','O','U']
consoanteMini = ['b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'x', 'w', 'y', 'z']
consoanteMai = ['B','C','D','F','G','H','J','K','L','M','N','P','Q','R','S','T','V','X','Y','W','Z']
digito = ['0', '1', '2', '3' , '4', '5', '6', '7', '8', '9']

verifica(letra, []) = False
verifica(letra, h:t)
    | letra == h = True
    | otherwise = verifica(letra, t)

letra(h:t) = h

tipo([]) = []
tipo(h:t)
    | verifica(letra(h),vogaisMini) = "Vogal Minuscula" : tipo(t)
    | verifica(letra(h),vogaisMai) = "Vogal Maiuscula" : tipo(t)
    | verifica(letra(h),consoanteMini) = "Consoante Minuscula" : tipo(t)
    | verifica(letra(h),consoanteMai) = "Consoante Maiuscula" : tipo(t)
    | verifica(letra(h),digito) = "Digito" : tipo(t)
    | otherwise = "Caractere Especial" : tipo(t)

contaVogal([]) = 0
contaVogal(h:t)
    | repete(h, vogaisMai) || repete(h, vogaisMini) = 1 + contaVogal t
    | otherwise = 0 + contaVogal t

maisVogal([], maior) = maior
maisVogal(h:t, maior)
    | contaVogal(h) > contaVogal(maior) = maisVogal(t,h)
    | otherwise = maisVogal(t,maior)

principal = do
    putStr "Digite sua lista de string (palavras separadas por espaco): "
    palavra <- getLine
    let lista = minhaLista(palavra)
    putStr "\na)Uma função para contar o número de caracteres que cada string possui sem repetir:\n"
    contaLista(lista)
    putStr "\n"
    putStr "\nb)Uma função que devolve uma lista contendo os tipos de caracteres que iniciam as strings da lista:\n"
    print(tipo(lista))
    putStr "\nc)Uma função que devolva a string que possui o maior número de vogais:\n"
    print(maisVogal(lista,[]))

    

