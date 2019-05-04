-- lab sheet one

import           Data.Char
import           Data.List

square :: Int -> Int
square x = x * x

pyth :: (Int,Int) -> Int
pyth (x, y) = square x + square y

isTriple :: Int -> Int -> Int -> Bool
isTriple x y z = pyth(x,y) == square z

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny x y z = if pyth(x,y) == square z then True else if pyth(x,z) == square y then True else if pyth(y,z) == square x then True else False
 

halfEvenNum :: Int -> Int
halfEvenNum x = if mod x 2 == 0 then div x 2 else x

halfEvens :: [Int] -> [Int]
halfEvens x = [halfEvenNum a | a <- x]

inRange :: Int -> Int ->[Int] -> [Int]
inRange x y z = [a | a <- z, a <= y, a >= x]

isPositive :: Int -> Bool
isPositive x = if x > 0 then True else False  

choosePositives :: [Int] -> [Int]
choosePositives x = [a| a <- x, isPositive a]

countPositives :: [Int] -> Int
countPositives x = length(choosePositives x)

capitalised :: String -> String 
capitalised x = (toUpper (head x)) : [toLower a | a <- (tail x)]


isFour :: String -> Bool
isFour x  = if length x >= 4 then True else False

stringToLower :: String -> String
stringToLower x = [toLower a | a <- x]

titleOp :: [String] -> String -> String
titleOp x y = if isFour y || ((head x) == y ) then capitalised y else stringToLower y

title :: [String] -> [String]
title [] = []
title x = [titleOp x a| a <- x]