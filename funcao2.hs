-- Função para converter a parte inteira de complemento de dois
binaryToInt :: [Int] -> Int
binaryToInt (signBit:bits) 
    | signBit == 0 = binaryToPositiveInt (signBit:bits)  -- Se o bit mais significativo for 0, o número é positivo
    | otherwise    = - (binaryToPositiveInt (complementoDeDois (signBit:bits)))  -- Se for 1, converte usando complemento de dois

-- Função para converter uma lista de bits positivos para Int
binaryToPositiveInt :: [Int] -> Int
binaryToPositiveInt = foldl (\acc bit -> acc * 2 + bit) 0

-- Função para calcular o complemento de dois
complementoDeDois :: [Int] -> [Int]
complementoDeDois = adicionaUm . map (1-)

-- Função para adicionar 1 ao complemento de dois
adicionaUm :: [Int] -> [Int]
adicionaUm = reverse . adicionaUmAux . reverse
  where
    adicionaUmAux [] = []
    adicionaUmAux (0:bits) = 1:bits
    adicionaUmAux (1:bits) = 0 : adicionaUmAux bits

-- Função para converter a parte fracionária
binaryToFractionalPart :: [Int] -> Double
binaryToFractionalPart bits = sum [fromIntegral bit / 2^i | (bit, i) <- zip bits [1..]]

-- Função principal para conversão (é esssa que chama no main, o resto é auxiliar dela)
bin2frac :: ([Int], [Int]) -> Double
bin2frac (integerPart, fractionalPart) = fromIntegral (binaryToInt integerPart) + binaryToFractionalPart fractionalPart