principal = do
     putStr "Digite o 1º numero: "
     a <- getLine
     putStr "Digite o 2º numero: "
     b <- getLine
     putStr "Digite o 3º numero: "
     c <- getLine
     putStr ("\nNumeros: "++show(read a::Int)++", "++show(read b::Int)++", "++show(read c::Int))
     putStr "\n"
     
     putStr ("Divisores - Quantas vezes repete\n")
     fatoracao(read a::Int,read b::Int,read c::Int,2, 0)
      
     --putStr ("Lista dos divisores: "++ show(fat((read a::Int),(read b::Int),(read c::Int),2,[a])) 
     putStr "\n\n"

--funcao para verificar se o numero é divisivel
verifica(a,n) = mod a n == 0

--funcao que devolve a lista com os numeros da fatoracao


fatoracao(a,b,c,n,cont)
     | a==1 && b==1 && c==1 = mostra(n, cont)
     | verifica(a,n) && verifica(b,n) && verifica(c,n) = fatoracao((div a n),(div b n),(div c n),n,cont+1)
     | verifica(a,n) && verifica(b,n) = fatoracao((div a n),(div b n),c,n,cont+1)
     | verifica(a,n) && verifica(c,n) = fatoracao((div a n),b,(div c n),n,cont+1)
     | verifica(b,n) && verifica(c,n) = fatoracao(a,(div b n),(div c n),n,cont+1)
     | verifica(a,n) = fatoracao((div a n),b,c,n,cont+1)
     | verifica(b,n) = fatoracao(a,(div b n),c,n,cont+1)
     | verifica(c,n) = fatoracao(a,b,(div c n),n,cont+1)
     | not(verifica(a,n)) && not(verifica(b,n)) && not(verifica(c,n)) = do 
                                                                           fatoracao(a,b,c,n+1,0)
                                                                           mostra(n, cont)
                        
mostra(n, cont)
  | cont > 0 = do
                putStr(show n)
                putStr " - "
                putStrLn(show cont)
  | otherwise = putStr ""
 
     
