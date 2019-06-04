import Data.Maybe
import Data.List

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

-- instance Eq Expr where
--     a == b = avalia a == avalia b

instance Eq Expr where
    Add a1 a2 == Add b1 b2 = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)
    Mult a1 a2 == Mult b1 b2 = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)
    Val a == Val b = a == b
    _ == _ = False


----------------------------
avalia (Val a) = a
avalia (Add a b) = avalia a + avalia b
avalia (Mult a b) = avalia a * avalia b

valSize (Val a) = 1
valSize (Add a b) = valSize a + valSize b
valSize (Mult a b) = valSize a + valSize b

operatorSize (Val a) = 0
operatorSize (Add a b) = 1 + operatorSize a + operatorSize b
operatorSize (Mult a b) = 1 + operatorSize a + operatorSize b

{--------------------------
            4
    2               6
1       3       5      7
--------------------------}

data Arvore a = Vazia | No (Arvore a) a (Arvore a) deriving (Show, Eq)

arvoreTeste = No (No (No Vazia 1 Vazia) 2 (No Vazia 3 Vazia)) 4 (No (No Vazia 5 Vazia) 6 (No Vazia 7 Vazia))


pertence x (Vazia) = False
pertence x (No esq a dir) =    x == a 
                            || pertence x esq 
                            || pertence x dir

pertenceTalvez :: Eq a => a -> Arvore a -> Maybe a
pertenceTalvez x a | pertence x a = Just x
                   | otherwise  = Nothing

vazia Vazia = True
vazia _ = False

buscaBinaria x (Vazia) = False
buscaBinaria x (No esq meio dir) | x == meio = True
                                 | x > meio = buscaBinaria x dir
                                 | otherwise = buscaBinaria x esq

buscaBinariaTalvez x arv | buscaBinaria x arv = Just x
                         | otherwise = Nothing


inserir Vazia elemento = No Vazia elemento Vazia
inserir (No a1 a a2) elemento |  a == elemento = No a1 a a2 
                              |  a > elemento = No (inserir a1 elemento) a a2
                              |  otherwise = No a1 a (inserir a2 elemento)






-- remover (No Vazia a Vazia) = Vazia
-- remover (No Vazia a dir) = dir
-- remover (No esq a Vazia) = esq

remove x Vazia = Vazia
remover x (No esq a Vazia) | x == a = esq
remover x (No Vazia a dir) | x == a = dir
remover x (No esq a dir) | x == a = No (remover b esq) b (remover b dir) 
                         | otherwise =  No (remover x esq) a (remover x dir) 
        where
            succ = sucessor (No esq a dir)
            No e b d = succ

menor Vazia = Vazia
menor (No esq _ _) | menor esq == Vazia = esq
                   | otherwise = menor esq

sucessor Vazia = Vazia
sucessor (No _ _ dir) = menor dir 



emOrdem (Vazia) = []
emOrdem (No esq a dir) = emOrdem esq ++ [a] ++ emOrdem dir


preOrdem (Vazia) = []
preOrdem (No esq a dir) = [a] ++ preOrdem esq ++ preOrdem dir


posOrdem (Vazia) = []
posOrdem (No esq a dir) = posOrdem esq ++ posOrdem dir ++ [a]


criarArvore [] = Vazia
criarArvore [x] = No Vazia x Vazia 
criarArvore lista = No (criarArvore esquerda) mediana (criarArvore direita)
    where 
        listaOrdenada = sort lista
        medianaIndice = (length lista) `div` 2
        esquerda = take (medianaIndice) listaOrdenada
        direita = drop (medianaIndice) listaOrdenada
        mediana = listaOrdenada !! medianaIndice

