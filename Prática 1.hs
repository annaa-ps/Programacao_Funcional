--PF: PRATICA 1 (Exercícios 3 ao 8) 

--Ex: 3 
dobro :: Int ->Int
dobro x = x * 2 

--Ex: 4
quadruplo :: Int->Int 
quadruplo x = dobro (dobro x)

--Ex: 5 
soma2 :: Int->Int->Int
soma2 x y = x+y 

--Ex: 6
soma4 :: Int->Int->Int->Int->Int
soma4 x y z w = soma2 (soma2 x y) (soma2 z w)

--Ex: 8
hipotenusa :: Float->Float->Float
hipotenusa a b = sqrt(a*a + b*b)
