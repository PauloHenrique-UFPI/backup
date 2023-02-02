import System.Random (randomRIO)


fimL cont = mod cont 3 == 0
proximo(h:r) = h

anaAssento(h:t, laux, nAna, cont)
    | cont == nAna = h
    | fimL cont = anaAssento(laux, laux, nAna, cont+1)
    | otherwise = anaAssento(t, laux, nAna, cont+1)

beaAssento(h:t, laux, ana, nBea, cont)
    | cont == nBea && h /= ana = h
    | cont == nBea && fimL cont  = proximo laux
    | cont == nBea = proximo t
    | fimL cont = beaAssento(laux, laux, ana, nBea, cont+1)
    | otherwise = beaAssento(t, laux, ana, nBea, cont+1)

carolAssento(h:t, ana, bea, cont)
    | h /= ana && h /= bea = h
    | otherwise = carolAssento(t, ana, bea, cont+1)

principal = do
    let lista = [1,0,2]
    putStr "Numero sorteado por Ana: "
    nAna <- randomRIO (1,100::Int)
    print nAna
    putStr "Ana sentou na cadeira: "
    let ana = anaAssento(lista, lista, nAna, 0)
    print ana

    putStr "Numero sorteado por Beatriz: "
    nBea <- randomRIO (1,100::Int)
    print nBea
    putStr "Beatriz sentou na cadeira: "
    let bea = beaAssento(lista, lista, ana, nBea, 0)
    print bea
    
    putStr "Carolina sentou na cadeira: "
    print (carolAssento(lista, ana, bea, 0))
    
    
   