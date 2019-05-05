-- lab sheet 4
import Data.Char
import Data.List


myWords :: String -> [ String ]
myWords [ ] = [ ]
myWords xxs @ (x : xs) | isSpace x = myWords xs
                         | otherwise = word : myWords rest
                         where (word, rest) = break isSpace xxs

unWords :: [ String ] -> String
unWords [ ] = [ ]
unWords xs = init (foldr (\x y -> x ++ " " ++ y) [ ] xs)                         

