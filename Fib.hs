module Fib where

import Data.List

fib :: Int -> Maybe Int
fib n = (fmap (snd . fst)) $ find (\(_,i) -> n == i) (zip res [0..])

res :: [(Int, Int)]
res = iterate f (0,1)

f :: (Num a, Num b) => (a, a) -> (a, a)
f (x, y) = (y,x+y)