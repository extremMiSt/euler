module Main where

fib :: [Integer]
fib = 1:2: zipWith (+) fib (tail fib)

fibfilter :: [Integer]
fibfilter = takeWhile (< 4000000) (filter even fib)


main :: IO ()
main = do
    print (sum fibfilter)