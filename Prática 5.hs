-- Pratica 5
-- Ex: 1
import Data.Char 
converte :: Char->(Char,Char,Int)
converte char = (char, toUpper char, ord char)

--Ex: 2
equacao :: (Float,Float,Float)->(Float,Float)
equacao (a,b,c)
   |delta >= 0 = (x1,x2)
   |otherwise = error("nao ha raizes reais")
  where delta = b^2-4*a*c 
        x1 = (-b+sqrt delta)/2*a
        x2 = (b+sqrt delta)/2*a

-- Ex: 3 
type Nome = String
type Idade = Int
type Sexo = Char
type Pessoa = (Nome, Idade, Sexo)

pessoa :: Int -> Pessoa
pessoa 1 = ("Otavio", 20, 'm')
pessoa 2 = ("Fernanda", 25,'f')
pessoa 3 = ("Anna", 19, 'f')

pessoa :: Int -> Pessoa
pessoa indice
   |indice == 1 = ("Otavio", 20, 'm')
   |indice == 2 = ("Fernanda", 25,'f')
   |indice == 3 = ("Anna", 19, 'f')

retorna_Idade :: Pessoa -> Idade
retorna_Idade (_,idade,_) = idade 

somaIdades :: Int -> Float
somaIdades n 
   | n == 1 = retorna (pessoa 1)
   | otherwise = retorna_Idade(pessoa n) + somaIdades (n-1)

mediaIdades :: Float -> Float
mediaIdades n = (somaIdades n)/n
