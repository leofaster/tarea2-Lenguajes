-- Tarea 2
-- Grupo 11:
-- Roberto Omaña 06-39990
-- Leopoldo Pimentel 06-40095

module Tarea2 where

import Data.List (insert)

--1.
--a)
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

-- insert :: Ord a => a -> [a] -> [a]
-- insert e ls = insertBy (compare) e ls
-- 
-- -- | The non-overloaded version of 'insert'.
-- insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
-- insertBy _   x [] = [x]
-- insertBy cmp x ys@(y:ys')
--  = case cmp x y of
--      GT -> y : insertBy cmp x ys'
--      _  -> x : ys

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
   
altura :: ArbolRosa a -> Int
altura (ArbolRosa _ []) = 0
altura r@(ArbolRosa e xs) = 1 + maximum(map altura (hijos r))

arbolito = ArbolRosa{elemento = 3, hijos = [ArbolRosa{elemento = 5, hijos = [ArbolRosa{elemento = 1, hijos = []}]},
   ArbolRosa{elemento = 6, hijos = []}, ArbolRosa{elemento = 7, hijos = []}]}