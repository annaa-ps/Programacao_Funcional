-- Prática 7 
-- Ex: 1
-- a) 
data Forma = Circulo Float | Retangulo Float Float | Triangulo Float Float

redonda:: Forma -> Bool 
redonda (Circulo _ ) = True 
redonda (Retangulo _ _ ) = False 

--b) 
calcularArea:: Forma -> Float 
calcularArea (Circulo raio) = pi * raio * raio
calcularArea (Retangulo b h) = b  * h

--c) 

--d) 
redonda:: Forma -> Bool 
redonda (Circulo _ ) = True 
redonda _ = False 

calcularArea:: Forma -> Float 
calcularArea (Circulo raio) = pi * raio *raio
calcularArea (Retangulo b h) = b * h
calcularArea (Triangulo b h) = (b * h)/2 


-- Ex: 2
--a) dobro x = x*2
--   ghci> :t dobro
--   dobro :: Num a => a -> a

-- b) aprovado nota = nota >= 6
--    ghci> :t aprovado
--    aprovado :: (Ord a, Num a) => a -> Bool

-- c) myLog x b = log x / log b
--    ghci> :t myLog
--    myLog :: Floating a => a -> a -> a

-- Ex: 3
--a) ghci> :t [[]]
--   [[]] :: [[a]]

--b) ghci> :t [[10,20,30], [], [5,6], [24]]
--   [[10,20,30], [], [5,6], [24]] :: Num a => [[a]]

--c) (10e-2, 20e-2, 30e-3)
--   (10e-2, 20e-2, 30e-3)
--   :: (Fractional a, Fractional b, Fractional c) => (a, b, c)

-- d) :t [(2,3), (4,5.6), (6,4.55)]
-- [(2,3), (4,5.6), (6,4.55)] :: (Num a, Fractional b) => [(a, b)]

--e)  :t (["bom","dia","brasil"], sum, drop 7 "Velho mundo")
-- (["bom","dia","brasil"], sum, drop 7 "Velho mundo")
--  :: (Foldable t, Num a) => ([String], t a -> a, [Char])

-- f) ghci> :t [sum, length]
--    [sum, length] :: Foldable t => [t Int -> Int]

--Ex: 4
type Coord = (Float, Float)
data Ponto = Coord Float Float deriving (Show)

soma:: Coord -> Coord -> Coord 
soma (xa,ya) (xb, yb) = (xa + xb, ya + yb)

soma' :: Ponto -> Ponto -> Ponto
soma' (Coord xa ya) (Coord xb yb) = Coord(xa+xb) (xb+yb)
