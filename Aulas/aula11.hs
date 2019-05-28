import System.IO
import Data.List
import Data.Function

data Ponto = Ponto Float Float deriving Show

data Shape = Retangulo Ponto Ponto | Circulo Ponto Float | Quadrado Ponto Ponto | Triangulo Ponto Ponto Ponto
    deriving (Show)

data Talvez a = Nada | Apenas a deriving Show

cabeca :: [a] -> Talvez a
cabeca [] = Nada
cabeca (x:xs) = Apenas x

bunda [] = Nada
bunda (x:xs) = Apenas xs

indiceDoElemento' x [] n = Nada
indiceDoElemento' x (y:ys) n | x == y = Apenas n
                             | otherwise = indiceDoElemento' x (ys) (n+1)
indiceDoElemento x ys = indiceDoElemento' x ys 0

divisao :: Integral a => a -> a -> Talvez a
divisao a 0 = Nada
divisao a b = Apenas (a `div` b)


distancia (Ponto x1 y1) (Ponto x2 y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)



area :: Shape -> Float
area (Circulo p r) = pi * (r^2)
area (Retangulo (Ponto x1 y1) (Ponto x2 y2)) = (x2 - x1) * (y2-y1)  
area (Quadrado (Ponto x1 y1) (Ponto x2 y2)) = (x2 - x1) * (y2-y1)  
area (Triangulo (Ponto x1 y1) (Ponto x2 y2) (Ponto x3 y3)) = ((x1 * y2 * 1) + (y1 * 1 * x3) + (1 * x2 * y3) - (x3 * y2 * 1) - (y3 * 1 * x1) - (1 * x2 * y1)) / 2

perimetro (Circulo p r) = 2 * pi * r
perimetro (Retangulo (Ponto x1 y1) (Ponto x2 y2)) = 
perimetro (Quadrado (Ponto x1 y1) (Ponto x2 y2)) = (x2 - x1) * (y2-y1)  
perimetro (Triangulo (Ponto x1 y1) (Ponto x2 y2) (Ponto x3 y3)) = ((x1 * y2 * 1) + (y1 * 1 * x3) + (1 * x2 * y3) - (x3 * y2 * 1) - (y3 * 1 * x1) - (1 * x2 * y1)) / 2



main = do
    a <- readFile "entrada.txt"
    let linhas = lines a
    let areas = sortBy (compare `on` area) (figuras linhas)
    let shapesFormatados = unlines . map (shapeFormatado) $ areas
    writeFile "saida.txt" shapesFormatados


shapeFormatado (Circulo (Ponto x1 y1) r) = "Circulo " ++ (unwords . map(show) $ [x1,y1,r])
shapeFormatado (Retangulo (Ponto x1 y1) (Ponto x2 y2)) = "Retangulo " ++ (unwords . map(show) $ [x1,y1,x2,y2])
shapeFormatado (Triangulo (Ponto x1 y1) (Ponto x2 y2) (Ponto x3 y3)) = "Triangulo " ++ (unwords . map(show) $ [x1,y1,x2,y2,x3,y3])

read_float :: String -> Float
read_float a = read a 

figuras [] = []
figuras (linha:linhas) | tipo == "Circulo" = circulo : figuras linhas
                       | tipo == "Retangulo" = retangulo : figuras linhas
                       | tipo == "Triangulo" = triangulo : figuras linhas
    where 
        tipo = head (words linha)
        propriedades = tail (map (read_float) (words linha))
        retangulo = Retangulo (Ponto (propriedades !! 0) (propriedades !! 1)) (Ponto (propriedades !! 2) (propriedades !! 3))
        circulo = Circulo (Ponto (propriedades !! 0) (propriedades !! 1)) (propriedades !! 2)
        triangulo = Triangulo (Ponto (propriedades !! 0) (propriedades !! 1)) (Ponto (propriedades !! 2) (propriedades !! 3)) (Ponto (propriedades !! 4) (propriedades !! 5))
