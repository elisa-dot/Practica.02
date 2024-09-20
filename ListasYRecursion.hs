{-Ejercicio 1-}
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

{-EJERCICIO 2-}
sumaLista :: Num a => [a] -> a
sumaLista [x] = x
sumaLista (x:xs) = x + sumaLista xs

{-Ejercicio 3-}
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento (x:xs) a True =  a:(x:xs)
agregaElemento (x:xs) a False = (x:xs)++[a]

{-EJERCICO 4-}

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = 0
maximoLista (x : xs)=
    if x > maximoLista xs
        then x
        else maximoLista xs

{-Ejercicio 5-}
indice :: [a] -> Int -> a
indice [] index = error "No hay elementos"
indice (x:xs) index = if index==0 
                        then x
                        else indice xs (index -1)
{-EJERCICIO 6-}

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n],n `mod` x == 0]

{-ejercicio 7-}
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x:conjunto[y | y <- xs, x/=y]

{-EJERCICIO 8-}

numerosPares :: [Int] -> [Int]
numerosPares lista = [x | x <- lista, x `mod` 2 == 0]