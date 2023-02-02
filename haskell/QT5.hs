
existeEm([], item) = False
existeEm(h:t, item)
    | item == h  = True
    | otherwise = existeEm(t, item)

uniao([], lista) = []
uniao(h:t, lista)
    | existeEm(lista,h) = uniao(t, lista)
    | otherwise = h : uniao(t, lista)

 
somaQ([],[],quad) = []
somaQ([],c:r,quad) = []
somaQ(h:t,[],quad) = []
somaQ(h:t,c:r,quad)
    | (h^2+c^2) > quad = (h^2+c^2) : somaQ(t,r,quad)
    | otherwise = somaQ(t,r,quad)

somaCubo(h:t,c:r) = h^3+c^3

principal(a, b) = do 
    putStr "a) uma função que devolva uma lista contendo a união ordenada entre (A  B) e (B  A)."
    putStr "\nA: "
    print(show(uniao(a,b)))
    putStr "B: "
    print(show(uniao(b,a)))

    putStr "\nb)Uma função que devolva uma lista contendo a soma entre os quadrados dos elementos das duas listas\n"
    putStr "que forem maiores do que a soma entre o cubo dos dois primeiros elementos da lista.\n"
    print(show(somaQ(a,b,somaCubo(a,b))))