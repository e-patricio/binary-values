module Main where
--Funcao 1
bincompl2dec :: [Int] -> Int
bincompl2dec bits
  | head bits == 1 = decimalValue - 2^n  
  | otherwise      = decimalValue    
  where
    n = length bits
    bitsWithPowers = zip bits [n-1, n-2 .. 0]
    decimalValue = sum [bit * 2^power | (bit, power) <- bitsWithPowers]

--Funcao 2
bin2frac :: ([Int], [Int]) -> Double
bin2frac (integerPart, fractionalPart) = fromIntegral integerValue + fractionalValue
  where
    integerValue = bincompl2dec integerPart
    fractionalValue = sum [fromIntegral bit / 2^i | (bit, i) <- zip fractionalPart [1..]]
  
--Funcao 3
somarbin :: [Int] -> [Int] -> Int -> [Int]
somarbin xs ys n = drop (length result - n) result
  where
    somarComCarry :: [Int] -> [Int] -> Int -> [Int]
    somarComCarry [] [] carry = if carry == 0 then [] else [carry]
    somarComCarry (x:xs) (y:ys) carry =
      bit : somarComCarry xs ys novoCarry
      where
        soma = x + y + carry
        bit = soma `mod` 2
        novoCarry = soma `div` 2
    somarComCarry _ _ _ = []

    result = reverse (somarComCarry (reverse xs) (reverse ys) 0)
    
--Funcao 4
subtrairbin :: [Int] -> [Int] -> Int -> [Int]
subtrairbin xs ys n = drop (length result - n) result
  where
    subtrairComBorrow :: [Int] -> [Int] -> Int -> [Int]
    subtrairComBorrow [] [] borrow = if borrow == 0 then [] else [1]
    subtrairComBorrow (x:xs) (y:ys) borrow =
      bit : subtrairComBorrow xs ys novoBorrow
      where
        diff = x - y - borrow
        (novoBorrow, bit)
          | diff < 0  = (1, diff + 2)
          | otherwise = (0, diff)
    subtrairComBorrow _ _ _ = []

    result = reverse (subtrairComBorrow (reverse xs) (reverse ys) 0)

main :: IO ()
main = do
  -- Teste para a função bincompl2dec
  let bin1 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1]    
  let result1 = bincompl2dec bin1
  putStrLn $ "Resultado de bincompl2dec para " ++ show bin1 ++ ": " ++ show result1

-- Teste para a função bin2frac (conversão de número binário com parte fracionária)
  let inteiro = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] 
  let fracionario = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0] 
  let result4 = bin2frac (inteiro, fracionario)
  putStrLn $ "Resultado de bin2frac para " ++ show inteiro ++ " e parte fracionária " ++ show fracionario ++ ": " ++ show result4

  -- Teste para a função somarbin
  let bin2 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]    
  let bin3 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]   
  let result2 = somarbin bin2 bin3 4  
  putStrLn $ "Resultado de somarbin para " ++ show bin2 ++ " + " ++ show bin3 ++ ": " ++ show result2

  -- Teste para a função subtrairbin
  let result3 = subtrairbin bin2 bin3 4  
  putStrLn $ "Resultado de subtrairbin para " ++ show bin2 ++ " - " ++ show bin3 ++ ": " ++ show result3
