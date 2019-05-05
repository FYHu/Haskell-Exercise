-- lab sheet 2
import Data.Char
import Data.List

inRange :: Int -> Int -> [Int] -> [Int]
inRange n m [] = []
inRange n m (x:xs) | (n <= x) && (x <= m) = x : inRange n m xs 
                   | otherwise = inRange n m xs

countPositive :: [Int] -> [Int]
countPositive [] = []
countPositive (x:xs) | (x > 0) = x : countPositive xs
                      | otherwise = countPositive xs

countPositives :: [Int] -> Int
countPositives x = length(countPositive x)

subStringToLower :: String -> String
subStringToLower [] = []
subStringToLower (x:xs) =  (toLower x): subStringToLower xs 

capitalised :: String -> String
capitalised (x:xs) = (toUpper x):(subStringToLower xs) 

capLong :: [Char] -> [Char]
capLong [ ] = [ ]
capLong word | length word >= 4 = word
             | otherwise = capitalised word

title :: [[Char]] -> [[Char]]
title [ ] = [ ]
title (w : words) = (capitalised w) : ( titleLower words)
    where titleLower [ ] = [ ]
          titleLower (w : words) = (capLong w) : (titleLower words)

          