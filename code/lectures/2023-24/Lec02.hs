import Prelude hiding (Bool, True, False, String)

-- Types in Haskell
-- All types are spelt with an uppercase letter at the start.
x :: Int
x = 3

-- Arbitrary-precision integers
y :: Integer
y = 2^2000

-- Characters with single quotes

-- Strings with double quotes
--mystring::String
--mystring = "hello"

-- Strings are just lists of characters, which we can state with a
-- type synonym. Like C typedef.
type String = [Char]

-- Haskell is very strict about types, there's no coercion
-- between numbers of different types (for example)
d::Double
d = 5.0

-- This won't work
d2::Double
d2 = fromIntegral x + d

-- But we can turn "ints" into Doubles

-- There are also tuple types, whose type _does_ encode the size (in contrast to lists)
tuple::(Int,Char,Int)
tuple = (1,'a',1)

-- Let's create our own types!
-- Bool is a type with two values, False and True.
-- The values are said to _inhabit_ the type
data Bool = False | True
    deriving Show

-- Similarly to mathematics, Haskell uses -> to indicate a function
-- type.
-- Now let's look at function types, you may not think about this
-- particularly in other languages, but it's pretty core in Haskell.

-- Let's implement logical operations on our Bool type.
-- A binary function "xor", whose truth table is
--
--   F F => F
--   T F => T
--   F T => T
--   T T => F
xor :: (Bool,Bool) -> Bool
xor (False,False) = False
xor (True,False) = True
xor (False,True) = True
xor (True,True) = False

main :: IO ()
main = 
    do
        print "hello"
        print (xor (True,False))

 
