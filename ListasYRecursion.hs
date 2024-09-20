--1. Longitud de una lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3. Agrega elementos a una lista
agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento (x:xs) a True =  a:(x:xs)
agregaElemento (x:xs) a False = (x:xs)++[a]

--5. Recuperar un elemento de una lista de acuerdo a su índice
indice :: [a] -> Int -> a
indice [] index = error "No hay elementos"
indice (x:xs) index = if index==0 
                        then x
                        else indice xs (index -1)

--7. Convertir una lista en conjunto
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x:conjunto[y | y <- xs, x/=y]