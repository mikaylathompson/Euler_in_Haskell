-- | Main entry point to the application.
module Main where

euler1 :: Integral a => a -> a
euler1 max = foldl (+) 0 (filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) [1..max-1])

euler2 :: Int -> Int
euler2 max = foldl (+) 0 (filter even (fibs_up_to max [1,1]))

fibs_up_to max xs = if (last xs + last(init xs)) >= max
                       then xs
                       else let x0 = last xs
                                in let x1 = last(init xs)
                                       in fibs_up_to max (xs ++ [x0 + x1])

    
-- | The main entry point.
main :: IO ()
main = do
    print $ euler2 4000000
