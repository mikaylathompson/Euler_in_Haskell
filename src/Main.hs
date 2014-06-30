-- | Main entry point to the application.
module Main where

euler1 :: Integral a => a -> a
euler1 max = foldl (+) 0 (filter (\x -> (mod x 3 == 0) || (mod x 5 == 0)) [1..max-1])


    
-- | The main entry point.
main :: IO ()
main = do
    print $ euler1 10
    print $ euler1 1000
