module Basics where

import Prelude hiding(length)
import Data.Char(toUpper)

isHaskellFun :: Bool
isHaskellFun = True

hundred :: Int
hundred = 100

noExplicitType = "Look ma, no type!"

twoThings :: (Int, Bool)
twoThings = (1, True)

ints :: [Int]
ints = 1:2:3:4:5:[]

haskellAdjectives :: [String]
haskellAdjectives = ["fun", "type-safe", "recursive", "wtf"]

plusOne :: Int -> Int
plusOne x = x + 1

plusOne' :: Integer -> Integer
plusOne' x = x + 1

-- What if we want to loop?
plusOneList :: [Int] -> [Int]
plusOneList []     = []
plusOneList (x:xs) = plusOne x : plusOneList xs

length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs 

yell :: String -> String
yell []     = "!"
yell (x:xs) = toUpper x : yell xs