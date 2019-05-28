data Nat = Zero | Succ Nat deriving Show

-- naturalParaInteiro' a n = case a of
--                          Zero -> n
--                          Succ b -> naturalParaInteiro' b (n+1)
-- naturalParaInteiro a = naturalParaInteiro' a 0

naturalParaInteiro' (Zero) n = n 
naturalParaInteiro' (Succ a) n = naturalParaInteiro' a (n+1)
naturalParaInteiro a = naturalParaInteiro' a 0

inteiroParaNatural 0 = Zero
inteiroParaNatural n = Succ(inteiroParaNatural (n-1))


somarFacil a b = inteiroParaNatural (naturalParaInteiro a + naturalParaInteiro b)


-- somarElegante (Zero) (Zero) = Zero
-- somarElegante (Zero) (Succ b) = Succ(somarElegante Zero b)
-- somarElegante (Succ a) (Zero) = Succ(somarElegante a Zero) 
-- somarElegante (Succ a) (Succ b) = Succ(Succ (somarElegante a b)) 

somarEleganteDeVerdade (Zero) n = n
somarEleganteDeVerdade (Succ a) b = Succ(somarEleganteDeVerdade a b)


data Expr = Val Int | Add Expr Expr | Mult Expr Expr 
            deriving Show



avalia (Val a) = a
avalia (Add a b) = avalia a + avalia b
avalia (Mult a b) = avalia a * avalia b

exprSize (Val a) = 1
exprSize (Add a b) = exprSize a + exprSize b
exprSize (Mult a b) = exprSize a + exprSize b

operatorSize (Val a) = 0
operatorSize (Add a b) = 1 + operatorSize a + operatorSize b
operatorSize (Mult a b) = 1 + operatorSize a + operatorSize b
