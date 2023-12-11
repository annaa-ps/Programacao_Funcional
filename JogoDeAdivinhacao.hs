-- MENU
module Main (main) where
import System.IO (stdout,hSetBuffering, BufferMode(NoBuffering))
import System.Random 

-- FunÃ§Ã£o que inicia o jogo
main:: IO()
main = do hSetBuffering stdout NoBuffering 
          putStrLn "\n-------------Seja Bem-Vindo ao Jogo de Adivinhacao!---------------\n"
          putStrLn "                     +---------------------+"
          putStrLn "                     | Escolha uma opcao:  |"
          putStrLn "                     |  1-Jogar            |"          
          putStrLn "                     |  2-Sair             |"
          putStrLn "                     +---------------------+"
          opcao<-readLn 
          case opcao of
            1 -> do  
                   putStrLn "\nVAMOS COMEÃ‡AR!"
                   putStrLn "Estou pensando em um numero de 1 a 100, voce consegue adivinhar qual eh?"
                   funcaoNumero
            2 -> putStrLn "Saindo..."
            _-> putStrLn "Opcao invalida! Digite 1 ou 2."

-- FunÃ§Ã£o que sorteia um nÃºmero aleatÃ³rio de 1 a 100
funcaoNumero :: IO()
funcaoNumero = do 
    numAleatorio <- randomRIO (1,100) :: IO Int 
    print numAleatorio
    adivinheNumero numAleatorio 1

-- FunÃ§Ã£o do jogo, que verifica se o jogador ganhou ou nÃ£o
adivinheNumero :: Int->Int->IO()
adivinheNumero numAleatorio tentativas = do 
         putStrLn ("\nTentativa " ++ show (tentativas)++":")
         putStrLn "Digite seu palpite:"
         numeroStr <- getLine 
         let numero = read numeroStr :: Int 
         if numero == numAleatorio 
          then do
                putStrLn ("Parabens! Voce acertou em " ++ show (tentativas)++ " tentativas")
                recorde tentativas
                reiniciarJogo 
          else if numero < numAleatorio 
           then do 
                 putStrLn "Seu palpite esta abaixo do numero correto!\n"
                 adivinheNumero numAleatorio (tentativas + 1)  
          else if numero > numAleatorio
            then do
                  putStrLn "Seu palpite esta acima do numero correto!"
                  adivinheNumero numAleatorio (tentativas + 1)
          else putStrLn "Erro!"

-- FunÃ§Ã£o que verifica se o jogador quer jogar novamente 
reiniciarJogo :: IO()
reiniciarJogo = do 
  putStrLn "\n--------------------------------"
  putStrLn "Deseja jogar novemente? (s/n)"
  putStrLn "--------------------------------"
  resposta <- getLine 
  if resposta == "s" || resposta == "S"
    then main 
    else putStrLn "Obrigado por jogar! Saindo..."

-- FunÃ§Ã£o que atualiza os recordes 
recorde :: Int -> IO ()
recorde novoRecorde = do
    recordeAnterior <- readFile "highscore.txt"
    let recordeAtualizado = read recordeAnterior
    if novoRecorde < recordeAtualizado
        then do
            writeFile "highscore.txt" (show novoRecorde)
            putStrLn "VocÃª bateu o recorde!"
        else putStrLn "Recorde nÃ£o batido."
