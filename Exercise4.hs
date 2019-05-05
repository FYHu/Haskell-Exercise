-- lab sheet 4
import Data.Char
import Data.List


words :: String -> [ String ]
words [ ] = [ ]
words xxs @ (x : xs) | isSpace x = words xs
                     | otherwise = word : words rest
                      where (word, rest) = break isSpace xxs

unWords :: [ String ] -> String
unWords [ ] = [ ]
unWords xs = init (foldr (\x y -> x ++ " " ++ y) [ ] xs)                         

