-- PRATICA 3
--Ex: 1
palindromo::IO()
palindromo = do putStrLn "Digite uma frase"
                frase <- getLine
                if frase == reverse frase
                then putStrLn "Eh um palindromo"
                else putStrLn "Nao eh um palindromo"
--Ex: 2
produto::IO()
produto = do putStrLn "Digite tres numeros float"
             num1 <- readLn :: IO Float
             num2 <- readLn :: IO Float
             num3 <- readLn:: IO Float
             putStrLn ("O produto eh:" ++ show(num1 *num2 * num3))
--Ex 3

celsius :: Dlouble -> Dlouble
celsius tf = 5/9 * (tf-32)

main :: IO()
main = do putStrLn "Digite uma temperatura em Fahrenheit"
          tempF <- readLn::IO Dlouble
          putStrLn ("Temperatura em Fahrenheit: "++ show tempF)
          putStrLn ("Temperatura em Celsius:" ++ show (celsius tempF))

-- Pratica 3
-- Ex: 4
calcularSituacao :: IO()
calcularSituacao = do putStrLn "Informe as tres notas:"
                      nota1<-readLn :: IO Float
                      nota2<-readLn :: IO Float 
                      nota3<-readLn :: IO Float 
                      let media = (nota1+nota2+nota3)/3
                      putStrLn (imprimeSituacao media)

imprimeSituacao:: Float -> String
imprimeSituacao media
    | media < 3 =  "Reprovado"
    | media > 3 && media <= 7 = "Exame especial"
    | otherwise = "Aprovado"


-- Ex: 5
classeEleitoral :: IO()
classeEleitoral = do putStrLn "Classe Eleitoral:"
                     putStrLn "-----------------"
                     putStrLn "Informe a sua idade:"
                     idade <- readLn :: IO Int 
                     putStrLn (verificarIdade idade)

verificarIdade :: Int -> String
verificarIdade num 
       | num < 16  = "Nao eleitor"
       | num >= 18 && num <= 65 = "Eleitor obrigatorio"
       | otherwise = "Facultativo"


-- Ex: 6
menu :: IO()
menu = do putStrLn "1. Salvar frase em arquivo"
          putStrLn "2. Imprime o conteudo do arquivo"
          putStrLn "3. Sair"
          putStrLn "Digite uma opcao: "
          opcao <- readLn :: IO Int 
          case opcao of 
            1 -> do putStrLn "Digite uma frase"
                    frase <- getLine 
                    w_direta frase
                    putStrLn "Frase Salva"
            2 -> r_direta
            _ ->putStrLn "Opcao invalida"
            if opcao /= 3 then menu else putStrLn "Saindo..."

w_direta :: IO()
w_direta = writeFile "teste.txt" frase


r_direta :: IO
r_direta = do conteudo <- readFile "teste.txt"
              putStrLn conteudo
