somarbin :: [Int] -> [Int] -> Int -> [Int]
somarbin xs ys n = drop (length result - n) result
  where
    -- Função auxiliar recursiva que realiza a soma bit a bit com carry
    somarComCarry :: [Int] -> [Int] -> Int -> [Int]
    somarComCarry [] [] carry = if carry == 0 then [] else [carry]
    somarComCarry (x:xs) (y:ys) carry =
      bit : somarComCarry xs ys novoCarry
      where
        soma = x + y + carry
        bit = soma `mod` 2
        novoCarry = soma `div` 2
    somarComCarry _ _ _ = []  -- Para garantir que a recursão pare se os tamanhos das listas não baterem

    -- Inverte as listas, soma, e depois inverte o resultado
    result = reverse (somarComCarry (reverse xs) (reverse ys) 0)
