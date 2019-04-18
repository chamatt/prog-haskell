double x = x + x

quadruple x = double (double x)

factorial n = product[1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5,6,7,8,9,10]

ultimo [x] = x
ultimo xs = ultimo (tail xs)


inicio [x] = [] 
inicio (x:xs) =  [x] ++ inicio xs

