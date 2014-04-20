module List where

import Data.Char(toUpper) 
import Basics hiding (length) 
import Prelude hiding (foldr, map, sum, foldl, (!!), head, concat, Maybe(..))

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

-- So, alternatively:
plusOneList' :: [Int] -> [Int]
plusOneList' = undefined

yell' :: [Char] -> [Char]
yell' xs = map toUpper xs ++ "!"

-- what do we do if we want to yell all the things?
yellAll :: [String] -> [String]
yellAll xs = map (\ys -> map toUpper ys ++ "!") xs

lengthOfYells :: [String] -> [Int]
lengthOfYells xs = map (\x -> length (yell x)) xs

plusOneListPointFree :: [Int] -> [Int]
plusOneListPointFree = map plusOne

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

sum' :: [Int] -> Int
sum' = foldr (\x y -> x + y) 0

concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- UGLY 
head :: [a] -> a
head []     = error "empty list!"
head (x:xs) = x

(!!) :: [a] -> Int -> a
xs !! n
  | length xs < n = error "too short!"
  | otherwise = head $ take n xs

data Maybe a = Just a | Nothing deriving (Show, Eq)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x