
{-Ejercicio 1-}

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

{-Ejercicio 2-}
sumaLista :: Num a => [a] -> a
sumaLista [a] = a
sumaLista (x:xs) = x + sumaLista(xs)

{-Ejercicio 3-}
agregarElemento :: [a] -> a -> Bool -> [a]
agregarElemento lista a bool = 
         if bool 
        then  a:lista
        else lista++[a]

{-Ejercicio 4-}
maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "no se puede ingresar una lista vacía"
maximoLista [x] = x     
maximoLista (x:xs)=
        if x > maximoLista(xs)
        then x
        else maximoLista(xs)

{-Ejercicio 5-}
indice :: [a] -> Int -> a
indice [] index = error "no puedes ingresa una lista vacía"
indice (x:xs) index =  if  index < 0 || index > longitud(x:xs)-1
        then error "indice no válido"
        else  if index == 0
    then x
    else indice xs (index -1)


{-Ejercicio 6-}
divisores :: Int -> [Int]
divisores a = [x|x <- [1..a] , mod a x == 0]

{-Ejercicio 7-}
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x:conjunto[a|a<-xs,x/=a] 

{-Ejercicio 8-}
numerosPares :: [Int] -> [Int]
numerosPares xs =  [x| x<- xs, mod x 2 == 0]


