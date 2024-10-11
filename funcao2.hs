bincompl2dec :: [Int] -> Int
bincompl2dec bits
  | head bits == 1 = decimalValue - 2^n  -- Se for negativo (vai subtrair o maior valor certinho)
  | otherwise      = decimalValue        -- Se for positivo não precisa
  where
    n = length bits
    -- Associa cada bit à sua respectiva potência de 2
    bitsWithPowers = zip bits [n-1, n-2 .. 0]
    -- Soma todas as potênciações para descobrir o valor decimal :)
    decimalValue = sum [bit * 2^power | (bit, power) <- bitsWithPowers]

bin2frac :: ([Int], [Int]) -> Double
bin2frac (integerPart, fractionalPart) = fromIntegral integerValue + fractionalValue
  where
  -- Converte a parte inteira utilizando bincompl2dec
    integerValue = bincompl2dec integerPart
  -- Converte a parte fracionária
    fractionalValue = sum [fromIntegral bit / 2^i | (bit, i) <- zip fractionalPart [1..]]
