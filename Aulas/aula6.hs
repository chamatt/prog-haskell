-- LAMBIDA NO MEU SACO

odds n = filter (\x -> x > 10) . map (\x -> x*2 + 1) $ [0..n-1]

pares :: (Num a, Enum a) => a -> [a]
pares n = map (\x -> x*2 ) [0..n-1]

filterPairs xs = filter (\z -> z `mod` 2 == 0) xs

oddsTil n = filter (\n -> n `mod` 2 /= 0) [0..n]

produto = \x -> \y -> x * y


-- Sections
-- succ  :: Num a => a -> a
succ' = (1+)

-- square :: Num a => a -> a
square = (^2)

-- reciproco :: Num a => a -> a
recipro = (1/)

double = (*2)

halv :: (Num a, Fractional a) => a -> a
halv = (/2) 




-- Compreencao de lista

multiplosDe7 n = [x * 7 | x <- [1..n]]

tabuada x = [ x*b | b <- [1..10]]

dependentGen = [(x,y) | x <- [1..3],y <- [x..3]]


indiceL m n = [(i,j) | i <- [0..m-1], j<-[0..n-1]]

indiceC m n = [(i,j) | j <- [0..n-1], i <- [0..m-1]]







indicesTriangularSuperior m n = [(i, j) | i <- [1..m], j <- [i..n]]

indicesTriangularInferior m n = [(i, j) | i <- [1..m], j <- [1..i]]

indicesMatrizPorLinha m n = [(i,j) | i <- [1..m], j <- [1..n]]
indicesMatrizPorColuna m n = [(i,j) | j <- [1..n], i <- [1..m]]


-- concatenar lista de lista
concatList xss = foldl (\acc -> \cur -> acc ++ cur) [] xss
concatList' xss = [ x | xs <- xss, x <- xs]

-- Produto intern
produtoInterno [] [] = 0
produtoInterno (x:xs) (y:ys) = x * y + produtoInterno xs ys

produtoInterno' xs ys = sum [ (xs !! i) * (ys !! i) | i <- [0..(length xs)-1] ]

primeirosImpares n = take n [x | x <- [1..], odd x]

fatoresDe n = [x | x <- [1..n], n `mod` x == 0]

ePrimo n = fatoresDe n == [1, n]

primosMenoresQue n = [x | x <- [1..n-1], ePrimo x]

nPrimos n = take n [x | x <- [1..], ePrimo x]

ehPerfeito x = (sum.init.fatoresDe $ x) == x

perfeitosAte n = [x | x <- [1..n], ehPerfeito x]

nPerfeitos n = take n [x | x <- [1..], ehPerfeito x]

listaDeFibo n = take n (fibo 1 1)
nfibo n = last (listaDeFibo n)
fibo a b = a:fibo b (a+b)
