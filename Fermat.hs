fermat :: Int -> Maybe (Int, Int)
fermat n = go $ ceiling $ sqrt (fromIntegral n)
   where go a = case (try n a) of j @ (Just _) -> j
                                  Nothing      -> go (a + 1) 

try :: Int -> Int -> Maybe (Int, Int)
try n a = fmap (\b -> ((a + b), (a - b))) result
  where result = get_perfect_sq (a^2 - n)

get_perfect_sq :: Int -> Maybe Int
get_perfect_sq x = if (sq * sq == x) then Just sq else Nothing
  where sq = floor $ sqrt (fromIntegral x :: Float)