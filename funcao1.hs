bincompl2dec :: [Int] -> Int
bincompl2dec bits =
  let n = length bits
      -- Função para converter binários (lista de bits) para decimal
      bin2dec :: [Int] -> Int
      bin2dec [] = 0
      bin2dec (x:xs) = x * 2^(length xs) + bin2dec xs
  in 
      -- Se o bit mais significativo (head) é 1, o número é negativo
      if head bits == 1 
         then bin2dec bits - 2^n  -- Subtrai 2^n para obter o valor em complemento de dois (vai subtrair o maior valor certinho)
         else bin2dec bits

-- Outra alternativa de resolução (tentei usar funções vistas em aula)
bincompl2dec :: [Int] -> Int
bincompl2dec bits
  | head bits == 1 = decimalValue - 2^n  -- Se for negativo (vai subtrair o maior valor certinho)
  | otherwise      = decimalValue        -- Se for positivo
  where
    n = length bits
    -- Associa cada bit à sua respectiva potência de 2
    bitsWithPowers = zip bits [n-1, n-2 .. 0]
    -- Soma todas as potênciações para descobrir o valor decimal :)
    decimalValue = sum [bit * 2^power | (bit, power) <- bitsWithPowers]
