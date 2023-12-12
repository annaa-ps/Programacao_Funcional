-- Pratica 4
-- Ex: 1
fatorialduplo::Int->Int
fatorialduplo n 
  | n == 0 = 1
  | n == 1 = 1
  | n > 0 = fatorialduplo (n-2)*n

-- Ex: 2
quociente :: Int -> Int -> Int
quociente p q
  | p < q = 0
  | otherwise = 1 + quociente(p-q) q 

resto:: Int -> Int -> Int
resto p q 
  | p < q = p
  | otherwise = resto (p-q) q

-- Ex: 3 
potencia:: Float -> Float -> Float
potencia _ 0 = 1
potencia n x = x * potencia x (n-1)

-- Ex: 4
nand:: Bool->Bool->Bool 
nand a b = if a == False || b == False
           then True else False 

nand'::Bool->Bool->Bool
nand' a b 
  | a == False || b == False = True 
  | otherwise = False
