decimalToBinary' 0 ordem = 0
decimalToBinary' num ordem = num `mod` 2 * (10^ordem) + decimalToBinary' (num `div` 2) (ordem+1)

decimalToBinary num = decimalToBinary' num 0


(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True


fat :: (Integral a, Ord a) => a -> a
fat 0 = 1
fat a = a * fat(a-1)


pow' :: (Integral a, Ord a) => a -> a -> a
pow' _ 0 = 1
pow' x y = x * pow' (x) (y-1)


-- List Patterns

head' (x:_) = x

tail' (_:xs) = xs


ultimo [x] = x
ultimo (x:xs) = ultimo xs

inicio [] = []
inicio [x] = [] 
inicio (x:xs) = x:(inicio xs)

-- tail pattern mathcing
rabo (x:xs) = xs


