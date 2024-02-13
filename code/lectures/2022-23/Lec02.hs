import Prelude hiding (Bool, True, False, String)

-- Types in Haskell
-- All types are spelt with an uppercase letter at the start.
x::Int 
x = 3

y::Int
y = 2+3

-- Arbitrary-precision integers
bignum::Integer
bignum = 2^2000

-- Characters with single quotes
someChar::Char
someChar = 'z'

-- Strings with double quotes
type String = [Char]

someString::String
someString = "test string"

-- Strings are just lists of characters, which we can state with a
-- type synonym. Like C typedef.
-- These lists are linked lists. And so the end of the string is that
-- the next pointer in the linked list data structure is empty.

-- Haskell is very strict about types, there's no implicit coercion
-- between numbers of different types (for example)
d::Double
d = 5.0

-- This won't work
--t1::Double
--t1 = d + x

-- But we can turn "ints" into Doubles
t2::Double
t2 = d + fromIntegral x

-- "xs has type list-of-Integer"
-- In general, for some t, xs :: [t] means "xs has type list-of-t"
xs::[Int]
xs = [1,2,3]

-- Here's a list of lists
-- Note how the type does not refer to the length.
xss::[[Int]]
xss = [[1,2],[1,2,3,4]]

-- There are also tuple types, whose type _does_ encode the size
tuple::(Int, Char, Int)
tuple = (1,'g',2)


-- Let's create our own types!
-- Bool is a type with two values, False and True.
-- The values are said to _inhabit_ the type
data Bool = True | False
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

-- How do we provide two arguments?
-- One option: take all the arguments at once,
-- packed up in a tuple.

xor :: (Bool,Bool) -> Bool
xor (False,False) = False
xor (True,False) = True
xor (False,True) = True
xor (True,True) = False

main :: IO ()
main = 
    do
        print "hello"
        print x
        print y
        print bignum
        print someString
        print someChar
        print (xor (False, True))
 
