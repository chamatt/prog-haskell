-- Zipar lista


zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

pairs' (x:xs) = zip' (x:xs) xs 

taOrdenada xs = and [ x <= y | (x,y) <- pairs' xs ]

produtoInterno xs ys = sum [x * y | (x,y) <- zip' xs ys]

ocorrenciasDe s xs = sum [ 1 | x <- xs, x == s ]

posicoesDe s xs = [ y | (x,y) <- zip' xs [0..], x == s ]

pyths n = [(z,y,x) | x <- [1..n], y <- [1..x], z <- [1..y], x^2 == y^2 + z^2 ]