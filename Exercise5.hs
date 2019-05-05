-- lab sheet 5

fac :: Int -> Int
fac 0 = 1
fac x | x < 0  = error "Fac input number should not less than 0"
      | otherwise = x * fac (x - 1)

fib :: Int -> Int 
fib 0 = 1
fib 1 = 1
fib x | x < 0 = error "Fib input number should not less than 0"
      | otherwise = fib(x -1) + fib(x - 2)      