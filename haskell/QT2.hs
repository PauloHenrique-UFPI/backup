quantidade(valor_peca, adicional, ingresso, valor, capacidade, qsecoes, lucro)
    | lucro > 0 = putStr("ingressos: ") >> print(ingresso) >> putStr("sessões: ") >> print(qsecoes)
    | lucro <= 0 && mod ingresso capacidade == 0 = quantidade(valor_peca, adicional, ingresso+1,valor, capacidade, qsecoes+1,lucro+valor-adicional)
    | otherwise = quantidade(valor_peca, adicional, ingresso+1, valor, capacidade, qsecoes,(lucro+valor))

lc(valor) = valor*(-1)
principal = do
    putStr "Digite o valor da peca: "
    a <- getLine
    putStr "Digite o valor por seccao: "
    b <- getLine
    putStr "Digite o valor do ingresso: "
    c <- getLine
    putStr "Digite a capacidade do local: "
    d <- getLine
    putStr "\n"
    
    putStr "O lucro vem: \n"
    quantidade(read a::Int, read b::Float, 0, read c::Float, read d::Int, 0, lc(read a::Float))
    putStr "\n\n"    