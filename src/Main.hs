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

euler3 :: Int -> Int
--euler3 num = last (filter (\x -> (0 == num `mod` x)) (primes_up_to (floor (sqrt (fromIntegral num))) [2]))
euler3 num = last (filter (\x -> (0 == num `mod` x)) (primes_up_to  num [2]))


primes_up_to :: Int -> [Int] -> [Int]
primes_up_to max xs = if (last xs) > max
                        then init xs
                        else primes_up_to max (xs++[next_prime (last xs + 1) xs])
                        
-- Returns the next prime, >= x, given the list of primes up to x
next_prime :: Int -> [Int] -> Int
next_prime x ps = if (is_prime x ps)
                        then x
                        else next_prime (x+1) ps

is_prime :: Int -> [Int] -> Bool
is_prime a [] = True
is_prime a ps = if (a `mod` head ps == 0)
                        then False
                        else is_prime a (tail ps)

-- | The main entry point.
main :: IO ()
main = do
    --print $ (filter (\x -> (0 == 42 `mod` x)) (primes_up_to 21 [2]))
    print $ euler3 190
    --print $ euler3 1000
    --print $ euler3 600851475143