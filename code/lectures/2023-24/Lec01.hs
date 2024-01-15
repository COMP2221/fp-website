-- Load standard library "Prelude" (loaded by default)
import Prelude

-- Naming rules:
-- Identifiers, e.g. variable and function names, consist of letters, numbers, _ (underscore) and ' (prime)
-- Variables and functions must start with a lower case letter or underscore
-- Types must start with an upper case letter

-- declare variables
-- can add type annotations for improved readability
x :: Integer
x = 3
y :: Integer
y = 4

-- variables in Haskell are immutable - they are defined once and cannot change
-- x = 5

-- unary function
id' :: Int -> Int
id' a = a

-- binary function
plus :: Int -> Int -> Int
plus a b = a + b

-- define lists
xs :: [Integer]
xs = [1,2,3,4,5,6]

xss :: [[Integer]]
xss = [[1,2],[3,4]]


-- Entry point of a Haskell program is called "main"
-- Specifies a sequence of events via do syntax
main :: IO ()
main =
    do
        -- hello world
        print "Hello, world!"
        -- function composition
        print (id' 3)
        print (plus 2 3)
        print (id'(id' 1))
        print (plus (id' 1) (id' 2))
        -- prefix to infix   
        print (1 `plus` 2)
        -- infix to prefix      
        print ((+) 1 2)
        -- list operations
        -- xs = [1,2,3,4,5,6]
        print (head xs)
        print (tail xs)
        print (take 3 xs)
        print (length xs)
        print (init xs)
        
