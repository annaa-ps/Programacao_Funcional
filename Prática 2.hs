--PF: PRATICA 2 (1 ao 4)
--Ex: 2
par :: Int->Bool 
par num = if mod num 2 == 0 then True else False

--Ex: 3
impar :: Int->Bool
impar num = not (par num)

--Ex: 4 
maior :: Int->Int->Int
maior a b = if a>b then a else b 

menor:: Int->Int->Int
menor a b = if a<b then a else b

--Ex: 4 
maior :: Int->Int->Int
maior a b = if a>b then a else b 

menor:: Int->Int->Int
menor a b = if a<b then a else b

diferenca::Int->Int->Int 
diferenca x y = l - s
               where l = maior x y
                     s = menor x y 
--Ex: 5
areaCirc::Float->Float
areaCirc diametro = pi*raio*raio 
         where raio = diametro/2 

--Ex: 6 
numRaizes:: Float->Float->Float->Int
numRaizes a b c 
   | delta > 0 = 2
   | delta == 0 = 1
   | otherwise = 0
   where delta = b*b - 4*a*c

--Ex: 7
menu::Float->Float->Int->Float
menu x y op = 
         case op of 
         1->x+y
         2->if x>y then x-y else y-x
         3->x*y
         4->if x>y && y/=0 then x/y else if y>=x && x/=0 then y/x else -1
         _->(-1)
