import Data.List

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

abs'' :: Int -> Int
abs'' n | n >= 0 = n
        | otherwise = -n

signum' :: (Ord a, Num a) => a -> a
signum' n | n < 0 = -1
          | otherwise = 1

{-
Comentario multilinha
-}

maior x y | x > y = x
          | otherwise = y


calculaMedia :: (Fractional a) => [a] -> a
calculaMedia xs = (sum xs) / fromIntegral (length xs)    
mediaMaiorQue5 xs | calculaMedia xs >= 5 = True
                  | otherwise = False

mediaMaiorQue5' xs | calculaMedia xs >= 5 = "Aprovado"
                   | otherwise = "Reprovado"

ordem x y | x < y = "Crescente"
          | y > x = "Decrescente"
          | otherwise = "Iguais"

crescente [x] = True
crescente (x:xs) | x <= head xs = crescente xs 
                 | otherwise = False

decrescente [x] = True
decrescente (x:xs) | x >= head xs = decrescente xs 
                   | otherwise = False

ordemLista xs | crescente xs = "Crescente"
              | decrescente xs = "Decrescente"
              | otherwise = "Desordenado"


maiorLista :: (Num a, Ord a) => [a] -> a
maiorLista [] = 0
maiorLista [x] = x
maiorLista (x:xs) | x > head xs = maiorLista ([x] ++ tail xs)
                  | otherwise = maiorLista (xs)

triangulo x y z  | ehTriangulo && ehEquilatero = "Triangulo Equilatero"
                 | ehTriangulo && ehIsosceles = "Triangulo Isosceles"
                 | ehTriangulo = "Triangulo Escaleno"
                 | otherwise = "Nao Eh Triangulo"
                   where 
                    ehTriangulo = and [x < y+x, y < x + z, z < x + y]
                    ehIsosceles = or [x == y, y == z, x == y]
                    ehEquilatero = and [x == y, y == z, x == y]

fatorial 0 = 1
fatorial x = x * fatorial (x-1)


fibonnaci x = take x (fib 0 1)
fib a b = [a] ++ fib b (a+b)



-- quicksort [] = []
-- quicksort (x:xs) = quicksort small ++ (x : quicksort large)
-- where small = [y | y <- xs, y <= x]
--         large = [y | y <- xs, y > x]

