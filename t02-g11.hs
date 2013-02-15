-- Tarea 2
-- Grupo 11:
-- Roberto Omaña 06-39990
-- Leopoldo Pimentel 06-40095

module Tarea2 where

import Data.List
import Data.Graph.Inductive.Internal.Heap hiding (insert)

--1.
--a)
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

--b)
partition :: (a -> Bool) -> [a] -> ([a],[a])
partition f xs = foldr (comparar f) ([],[]) xs

comparar :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
comparar f x (ys,zs) 
   | f x       = (x:ys,zs)
   | otherwise = (ys, x:zs)

--2.
data ArbolRosa a 
   = ArbolRosa { elemento :: a, hijos :: [ArbolRosa a]}
   deriving Show

--a)
altura :: ArbolRosa a -> Int
altura (ArbolRosa _ []) = 0
altura (ArbolRosa e xs) = 1 + maximum(map altura xs)

--b)
sumaArbol :: Num a => ArbolRosa a -> a
sumaArbol (ArbolRosa e xs) = e + (foldr (+) 0 (map sumaArbol xs))

--c)
aplanar :: ArbolRosa a -> [a]
aplanar (ArbolRosa e xs) = e:foldr (++) [] (map aplanar xs)

--d)
mapRosa :: (a -> b) -> ArbolRosa a -> ArbolRosa b
mapRosa f (ArbolRosa e []) = ArbolRosa (f e) []
mapRosa f (ArbolRosa e xs) = ArbolRosa (f e) [mapRosa f x | x <- xs]

--e) 
esHeap :: Ord a => ArbolRosa a -> Bool
esHeap (ArbolRosa e xs) = noHijos || (todosHeap && e > maxHijo)
  where
    maxHijos = map elemento xs
    sonHeaps = map esHeap xs
    noHijos = null xs
    maxHijo  = maximum maxHijos
    todosHeap = and sonHeaps


--3.
--b) y c)

foldRosa :: (a -> [b] -> b) -> ArbolRosa a -> b
foldRosa f (ArbolRosa e xs) = f e $ map (foldRosa f) xs

--d)
foldSuma :: Num a => ArbolRosa a -> a
foldSuma = foldRosa (\ a b -> a + sum b)

foldAltura :: ArbolRosa a -> Int
foldAltura = foldRosa (\ a b -> if null b then 0 else maximum b + 1)

foldAplanar :: ArbolRosa a -> [a]
foldAplanar = foldRosa (\ a b -> a : concat b)
 

--4.
--a)
unfoldrLista :: (b -> Maybe (a,b)) -> b -> [a] 
unfoldrLista f b = case f b of
  Nothing     -> []
  Just (a,bb) -> a:unfoldrLista f bb


--b)
factorizar :: Int -> [Int]
factorizar n = unfoldr funcion 1
  where
    funcion b =
      if null sigDivP
      then Nothing
      else Just (head sigDivP, head sigDivP + 1)
      where sigDivP = filter (> b) $ filter esPrimo $ divisores n
    
esPrimo :: Int -> Bool
esPrimo n = n == 1 || 2 == length [x | x <- [1..n], mod n x == 0]
 
--c)
aplanarUnfoldr :: ArbolRosa a -> [a]
aplanarUnfoldr arbol = unfoldr funcion [arbol]
  where
    funcion []     = Nothing
    funcion (x:xs) = Just (elemento x,  xs)

--d)
heapSort :: (Ord a) => [a] -> [a]
heapSort = (map fst) . transformarALista . build . map (\x->(x,x))

transformarALista :: Ord a => Heap a b -> [(a,b)]
transformarALista heap = unfoldr f heap
   where
      f Empty = Nothing
      f heap = Just (findMin heap, deleteMin heap)

--5.
--a)
unfoldArbolRosa :: (b -> (a, [b])) -> b -> ArbolRosa a
unfoldArbolRosa f b = ArbolRosa dato $ map (unfoldArbolRosa f) semillas
  where (dato, semillas) = f b

--b)
arbolDivisores :: Int -> ArbolRosa Int 
arbolDivisores a = unfoldArbolRosa f a
  where  
    f semilla = ( semilla ,  init . tail $ divisores semilla ) 
  
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]


arbolito = ArbolRosa{elemento = 3, hijos = [ArbolRosa{elemento = 5, hijos = [ArbolRosa{elemento = 1, hijos = []}]},
   ArbolRosa{elemento = 6, hijos = []}, ArbolRosa{elemento = 7, hijos = [ArbolRosa{elemento = 9, hijos = []}]}]}