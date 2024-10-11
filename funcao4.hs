subtrairbin :: [Int] -> [Int] -> Int -> [Int]
subtrairbin xs ys n = drop (length result - n) result
  where
    -- Função auxiliar recursiva que realiza a subtração bit a bit com borrow
    subtrairComBorrow :: [Int] -> [Int] -> Int -> [Int]
    subtrairComBorrow [] [] borrow = if borrow == 0 then [] else [1]  -- Se houver um borrow sobrando no final
    subtrairComBorrow (x:xs) (y:ys) borrow =
      let diff = x - y - borrow  -- Subtração com o borrow anterior
          (novoBorrow, bit)
            | diff < 0  = (1, diff + 2)  -- Se a diferença for negativa, empresta 1
            | otherwise = (0, diff)      -- Caso contrário, não há borrow
      in bit : subtrairComBorrow xs ys novoBorrow
    subtrairComBorrow _ _ _ = []  -- Para garantir que a recursão pare se os tamanhos das listas não baterem

    -- Inverte as listas, subtrai, e depois inverte o resultado
    result = reverse (subtrairComBorrow (reverse xs) (reverse ys) 0)
