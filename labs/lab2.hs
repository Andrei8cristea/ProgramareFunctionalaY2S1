import Data.List

myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x


maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y


max3 x y z = let
             u = maxim x y
             in (maxim  u z)

max4 w x y z = let
            a = maxim w x
            b = maxim y z
            in (maxim a b) 


squared_sum :: Integer -> Integer -> Integer
squared_sum x y = x*x+y*y

is_even :: Integer -> String
is_even x = if (x `mod` 2 == 0)
                then "par"
            else "impar"

factorial :: Integer -> Integer
factorial x = if (x == 1)
                then 1
            else if (x == 0)
                then 1
            else x*factorial(x-1)

is_greater_than_double :: Integer -> Integer -> Bool
is_greater_than_double x y = x > 2 * y

max_list :: (Ord a) => [a] -> a
max_list [x] = x
max_list (x:tails) = max x (max_list tails)

poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a * x^2 + b * x + c

eeny :: Integer -> String
eeny x = if (x `mod` 2 == 0)
                then "eeny"
            else "meeny"

fizzbuzz1 :: Integer -> String
fizzbuzz1 x = if(x `mod` 5 == 0 && x `mod` 3  == 0)
                then "FizzBuzz"
                else if(x `mod` 5 == 0)
                then "Buzz"
                else if(x `mod` 3 == 0)
                then "Fizz"
                else ""

fizzbuzz2 :: Integer -> String
fizzbuzz2 x
  | x `mod` 5 == 0 && x `mod` 3 == 0 = "FizzBuzz"
  | x `mod` 5 == 0 = "Buzz"
  | x `mod` 3 == 0 = "Fizz"
  | otherwise = ""




fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
tribonacci1 :: Integer -> Integer
tribonacci1 n
    | n == 1 = 1
    | n == 2 = 1
    | n == 3 = 2
    | otherwise = tribonacci1(n-1) + tribonacci1(n-2) + tribonacci1(n-3)

tribonacci2 :: Integer -> Integer
tribonacci2 1 = 1
tribonacci2 2 = 1
tribonacci2 3 = 2
tribonacci2 n = tribonacci2(n-1) + tribonacci2(n-2) + tribonacci2(n-3)

binomial1 :: Integer -> Integer -> Integer
binomial1 n k
    | n == 0 = 1
    | k == 0 = 0
    | otherwise = binomial1(n-1)k + binomial1(n-1) (k-1) 
