-- High Order Functions


-- funcao map

mapeia f [] = []
mapeia f (x:xs) = f x:(mapeia f xs) 

mapeia' f xs = [f x | x <- xs]

-- funcao filter

filtrar f [] = []
filtrar f (x:xs)  | f x = x:filtrar f xs
                  | otherwise = filtrar f xs

filtrar' f xs = [x | x <- xs, f x == True]

-- paraTodo

paraTodo f xs = and [f x | x <- xs]

paraTodo' f [x] = f x
paraTodo' f (x:xs) = f x && paraTodo' f xs

-- existe

existe f xs = or [f x | x <- xs]

existe' f [] = False
existe' f (x:xs) = f x || paraTodo' f xs


-- takeWhile e dropWhile
pegarEnquanto' f (x:xs) | f x = x:pegarEnquanto' f xs
                        | otherwise = []

-- largarEnquanto f (x:xs) | f x = []
--                         | otherwise = x:largarEnquanto f xs 

largarEnquanto f [] = []
largarEnquanto f (x:xs) | f x = largarEnquanto f xs 
                        | otherwise = xs


-- function composition operator

impar = not . even

segundo = head . tail

terceiro = head . tail . tail

ultimo = head . reverse


-- fold

foldRec:: (a -> b -> b) -> b -> [a] -> b
foldRec f i [] = i
foldRec f i (x:xs) = f x (foldRec f i xs)

-- soma
soma = foldRec (+) 0

-- produto
produto = foldRec (*) 1

-- or
ou = foldRec (||) False

-- tamanho

tamanho = foldRec (\cur acc -> acc + 1) 0

-- concatena

concatena = foldRec (++) [] 

-- inverter

inverter = foldRec (\cur acc ->  acc ++ [cur]) []

-- sequencia de igauis 


maiorSeqIgual[] = []
maiorSeqIgual (xs:xss)  | length xs >= length (maiorSeqIgual xss) = xs
                        | otherwise = maiorSeqIgual xss

listaDeSequenciasIguais [] = []
listaDeSequenciasIguais (x:xs) = [takeWhile (\y -> x == y) (x:xs)] ++ listaDeSequenciasIguais (dropWhile (\y -> x == y) (x:xs))

maiorSeqIg xs = maiorSeqIgual (listaDeSequenciasIguais xs)


-- sequencia generica

maiorSeq [] = []
maiorSeq (xs:xss)  | length xs >= length (maiorSeq xss) = xs
                   | otherwise = maiorSeq xss

-- listaDeSequencias :: (Eq a, Ord a) => (a -> a -> Bool) -> [a] -> [[a]]
listaDeSequencias f [] = []
listaDeSequencias f [x] = []
listaDeSequencias f (x:y:xs) = [x:takeWhile (\y -> f x y) (y:xs)] ++ listaDeSequencias (f) (dropWhile (\y -> f x y) (y:xs))


maiorSeqDe f xs = maiorSeq (listaDeSequencias f xs)




