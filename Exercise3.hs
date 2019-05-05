-- lab sheet 3
mult :: [Int] -> Int
mult x = foldr (*) 1 x

posList :: [Int] -> [Int]
posList xs = filter(\x -> x > 0) xs

trueList :: [Bool] -> Bool
trueList = foldr (&&) True

maxList :: Ord a => [a] -> a
maxList (x:xs) = foldr max x xs 

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b xs = filter (\x->(x>= a) && (x<= b)) xs

countPositives :: [Int] -> Int
countPositives xs = length (posList xs)

myLength :: [a] -> Int 
myLength x = foldr (+) 0 (map (\x -> 1) x)

myMap :: (a -> b) -> [a] -> [b]
myMap f as = foldr (\a ls -> (f a) : ls) [ ] as

myLength' :: [a] -> Int
myLength' = myLength