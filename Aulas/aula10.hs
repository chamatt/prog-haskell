import System.IO


main = menuInterativo



-- -- ler nome e printar reverso
printaInvertido :: IO ()
printaInvertido = do  putStrLn "Digite seu nome"
                      line <- getLine
                      putStrLn . reverse $ line

-- -- Menu que faz operacao
menu op xs  | op == 1 = xs ++ ["Somar"]
            | op == 2 =  xs ++ ["Subtrair"]
            | op == 3 =  xs ++ ["Multiplicar"]
            | otherwise =  xs ++ ["Inválido"]

leOp = do putStrLn "0 - Sair, 1 - Somar, 2 - Subtrair, 3 - Multiplicar"
          opcao <- getLine
          return (read opcao :: Int)

interativo xs = do op <- leOp
                   if op == 0 then return xs
                   else interativo (menu op xs)

menuInterativo = do
                x <- interativo []
                putStrLn (show x)


-- ler lista de inteiros separada por brancos e apresentar produto


convInt [] = []
convInt (x:xs) = (read (x) :: Int) : convInt xs

produtoInteiros = do 
                    line <- getLine
                    putStrLn . show . product . convInt . words $ line                    


-- ler a primeira linha e printa no arquivo saida

lerArquivo = do
                entrada <- openFile "entrada.txt" ReadMode
                saida <- openFile "saida.txt" WriteMode
                linha <- hGetLine entrada
                putStrLn linha
                hPutStrLn saida linha
                hClose entrada
                hClose saida

-- copia uma arquivo de um pra outro
lerArquivoPraCopiar = do
    entrada <- openFile "entrada.txt" ReadMode
    saida <- openFile "saida.txt" WriteMode
    copia entrada saida
    

copia entrada saida = do
         
         if (hIsEOF  hGetLine entrada) 
         then do
                hClose saida 
                hClose entrada
         else do
                linha <- hGetLine entrada
                hPutStrLn saida linha
                copia entrada saida


-- mamateando com a funcao
-- lerArquivoMamata = do
--     entrada <- openFile "entrada.txt" ReadMode
--     saida <- openFile "saida.txt" WriteMode
--     texto <- hGetContents entrada
--     putStrLn texto
--     hPutStrLn saida texto
--     hClose entrada
--     hClose saida

-- mamateando mais ainda
-- lerArquivoMamata2 = do
--     entrada <- openFile "entrada.txt" ReadMode
--     texto <- readFile entrada
--     writeFile "saida.txt" texto
