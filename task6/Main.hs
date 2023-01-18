module Main where

square :: Num a => a -> a
square n = n*n

main :: IO ()
main = do
    let ssq = sum $ map square [1..100]
    let sqs = square (sum [1..100])
    print $ sqs - ssq
    print "done"