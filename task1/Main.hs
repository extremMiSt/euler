module Main where

threefive :: Integral a => [a] -> [a]
threefive (s:ss) = if (s `mod` 3 == 0) || (s `mod` 5 == 0) 
    then
        s : threefive ss
    else
        threefive ss
threefive [] = []

main :: IO ()
main = do
    print (sum (threefive [1..999]))