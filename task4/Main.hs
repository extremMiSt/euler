{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import qualified Data.Set as S
import Data.Set (findMax)

data PointVal = PV Integer Integer
    deriving (Show)

instance Eq PointVal where
    (==) :: PointVal -> PointVal -> Bool
    (PV i1 j1) == (PV i2 j2) = (i1*j1) == (i2*j2)

instance Ord PointVal where 
  (<=) :: PointVal -> PointVal -> Bool
  (PV a1 b1) <= (PV a2 b2) = (a1*b1) <= (a2*b2)


isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (s:ss) | s == last ss = isPalindrome (init ss)
                    | otherwise = False

isPvPalindrome :: PointVal -> Bool
isPvPalindrome (PV n1 n2) = isPalindrome (show (n1*n2)) 


class Ord a => Graph b a where 
    neighbours :: b -> a -> [a]

instance Graph () PointVal where
  neighbours :: () -> PointVal -> [PointVal]
  neighbours set (PV a b) = concat [p1,p2,p3,p4,p5,p6]
    where 
        p1 = [PV (a-1) (b-1) | a >= 100, b >= 100, a>=b]
        p2 = [PV (a-1) b | a >= 100, a>=b]
        p3 = [PV a (b-1) | b >= 100, a>=b]
        p4 = [PV (a+1) (b+1) | a <= 999, b <= 999, a>=b]
        p5 = [PV (a+1) b| a <= 999, a>=b]
        p6 = [PV a (b+1) | b <= 999, a>=b]

largest :: (Graph b PointVal) => (PointVal -> Bool) -> b -> S.Set PointVal -> S.Set PointVal -> Maybe PointVal
largest p g visited q | null q = Nothing
largest p g visited q | e <- findMax q, e `elem` visited = largest p g visited (S.deleteMax q)
                      | e <- findMax q, p e = Just e
                      | e <- findMax q, otherwise = largest p g (S.insert e visited) (S.deleteMax q `S.union` S.fromList (neighbours g e))

main :: IO ()
main = do
    let (Just (PV a b)) = largest (isPvPalindrome) () S.empty (S.singleton (PV 999 999))
    print $ a*b
    print "done"