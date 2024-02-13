-- Load standard library "Prelude" (loaded by default)
import Prelude

-- Naming rules
-- Identifiers, e.g. variable and function names, consist of letters, numbers, _ (underscore) and ' (prime)
-- variables and functions must start with a lower case letter or underscore
-- Types must start with an upper case letter

-- Compute factorial
factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * (n-1)

-- Function definition
f1 :: Integer -> Integer
f1 x = x

f2 :: Integer -> Integer -> Integer
f2 x y = x + y

g :: Integer -> Integer
g x = -1*x

-- Layout rule
x = 5
y = 4

-- Naming convention for lists
xs = [1,2,3,4,5]

xss = [[1,2],[1,2,3]]

main :: IO ()
main = 
    do
        -- f(x)
        print (f1 3)
        -- g(f(x))
        print (g (f1 1))
        putStrLn "Hello, World!"
        print (factorial 4)
        -- List operations, e.g. reverse, init, tail, last
        print (head xs)
 
