module Main where

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

isqrt :: (Integral c, Integral a) => a -> c
isqrt n = (floor.sqrt) (fromIntegral n)

first :: Integer -> [Integer] -> Maybe Integer
first n (l:ls) | isqrt n > l = if (n `mod` l) == 0 then Just l else first n ls
              | otherwise = Nothing

fac :: Integer -> [Integer]
fac n | Nothing <- f = [n]
      | (Just d) <- f = d : fac (n `div` d)
    where
      f = first n primes

main :: IO ()
main = do
    let n = 600851475143
    print $ fac n
    print "done"