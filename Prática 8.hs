import Data.Char
-- PRÁTICA 8
-- Ex:1
--a) 
primeiros :: [(a,b)] -> [a]
primeiros ls = map fst ls 

-- b)
maiuscula :: String -> String 
maiuscula frase = map toUpper frase 

--c) 
dobro :: Num a => [a] -> [a]
dobro ls = map (*2) ls

--Ex:2 
--a) 
 pares :: [Int] -> [Int]
 pares ls = filter even ls

import Data.Char
--b) 
alfa :: String -> String 
alfa x = filter isAlpha x

--c) 
rm_char :: Char -> String -> String 
rm_char char frase = filter (/= char) frase

--Ex: 3
--a) 
produto :: Num a => [a] -> a 
produto ls = foldr (*) 1 ls 

--b) 
e_logico:: [Bool] -> Bool
e_logico ls = foldr (&&) True ls

--c) 
concatena :: [String] -> String
concatena ls = foldr (++) "" ls
