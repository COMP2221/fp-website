-- Curried functions

-- For functions of more than one argument, we have a choice of
-- whether to take the arguments one at a time (curried) or all at
-- once (uncurried). Haskell prefers the former.

add' :: (Int, Int) -> Int
add' (x, y) = x + y

add :: Int -> Int -> Int
add x y = x + y

-- A simple example as to why.
-- Consider adding 1 to every entry of a list
xs :: [Int]
xs = [1, 4, 5, 6]

-- Add 1 to every element of a list
ys :: [Int]
ys = map (add 1) xs


-- compare with the uncurried version
-- Here I explicitly had to introduce a name for the second argument
-- to add' because there's no way to construct the partial application
-- of add' to its first argument: I need to be able to construct the
-- whole tuple.
zs :: [Int]
zs = map (\x -> add' (1, x)) xs

-- Associativity and binding of -> and function application.

-- We now need some conventions on how tightly -> (abstraction) and function
-- application bind, and which way.
-- The natural consequence of curried function application is that ->
-- associates to the right: like this.
tripleProduct :: Int -> (Int -> (Int -> Int))
tripleProduct x y z = x * y * z

-- And function application associates to the left, like this.
xyz :: Int
xyz = (((tripleProduct 1) 2) 3)

-- With these conventions, we can remove all the brackets. Which makes
-- things a little cleaner to read. Although we should recall the way
-- to read this is as above with the parentheses.
tripleProduct' :: Int -> Int -> Int -> Int
tripleProduct' x y z = x * y * z

xyz' :: Int
xyz' = tripleProduct 1 2 3

main = do
    print ys
    print xyz'

