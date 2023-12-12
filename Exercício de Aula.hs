import Data.Char

-- a)Faça uma função compr, que retorne quantos elementos há na lista (seu comprimento)
-- b) Faça uma função pertence, que verifica se um elemento pertence à lista, retornando um Bool

-- a) 
compr:: [Int] -> Int 
compr [] = 0 
compr (h:t) = 1 + compr t


-- b)
pertence:: Int -> [Int] -> Bool
pertence _ [] = False
pertence x (h:t) = if x == h then True else pertence x t  


-- No módulo Char, encontramos a função toUpper, que converte uma letra minúscula na sua correspondente maiúscula
-- a)Faça uma função recursiva que receba uma lista de letras e retorne uma lista com todas as letras convertidas em suas correspondente maiúsculas 
-- b) Faça uma nova função que receba uma palavra e retorna numa tupla-2 a palavra
-- original e a sua correspondente escrita em maiúscula. Dica: Use a função anterior

-- a) 
maius:: String -> String
maius  "" = ""
maius (h:t) = [toUpper h] ++ maius t
