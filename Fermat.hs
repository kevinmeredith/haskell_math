module Fermat where

import Data.Set

factors :: Int -> Maybe [Int]
factors x = do 
    _   <- odd' x 
    res <- fermat x
    if (idAndItself x res) then (return [x]) else combine (factors (fst res)) (factors (snd res))

combine :: Maybe [a] -> Maybe [a] -> Maybe [a]
combine xs ys = xs >>= (\x -> fmap (++ x) ys)

tupleToList :: (a, a) -> [a]
tupleToList (x, y) = [x, y]

idAndItself :: (Eq a , Num a) => a -> (a, a) -> Bool
idAndItself a (x, y) = x == 1 && y == a || y == 1 && x == a

---- Only odd?
---- TODO: see #1 from http://math.stackexchange.com/questions/1267465/simple-fermat-factorization-example/1267467#comment2574629_1267467
--fermat :: Int -> Maybe (Int, Int)
fermat n = odd' n >> (go $ ceiling $ sqrt (fromIntegral n))	
   where go a = case (try n a) of j @ (Just _) -> j
                                  Nothing      -> go (a + 1) 

odd' :: Int -> Maybe Int
odd' x = if (odd x) then Just x else Nothing

try :: Int -> Int -> Maybe (Int, Int)
try n a = fmap (\b -> ((a + b), (a - b))) result
  where result = get_perfect_sq (a^2 - n)

get_perfect_sq :: Int -> Maybe Int
get_perfect_sq x = if (sq * sq == x) then Just sq else Nothing
  where sq = floor $ sqrt (fromIntegral x :: Float)