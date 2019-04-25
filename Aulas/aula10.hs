main :: IO ()

main = do  putStrLn "Digite seu nome"
           line <- getLine
           putStr . reverse $ line



