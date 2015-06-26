module PowerSet where

import Data.Set (fromList, toList)
import Data.List (sort)

f :: (Ord a, Eq a) => [a] -> [[a]]
f = (foldr (\e acc -> (powerset' e) ++ acc) []) . tailsPlus

tailsPlus :: [a] -> [[a]]
tailsPlus []           = []
tailsPlus xxs @ (_:xs) = xxs : tailsPlus(xs)

powerset :: (Ord a, Eq a) => [a] -> [[a]]
powerset xs = toList . fromList . (map sort) $ xs : [] : f xs ++ map return xs


powerset' :: (Ord a, Eq a) => [a] -> [[a]]
powerset' xs = do 
	x  <- xs
	y <- tailOrEmpty xs
	if(x == y) then [] else [[x,y]]

tailOrEmpty :: [a] -> [a]
tailOrEmpty []     = []
tailOrEmpty (_:xs) = xs

-- P([1,2,3]) = { {}, {a}, {b}, {c}, {a, b}, {a, c}, {b, c}, {a, b, c} }

