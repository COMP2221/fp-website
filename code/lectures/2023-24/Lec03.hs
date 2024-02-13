-- Currying

-- For functions of more than one argument, we have a choice of
-- whether to take the arguments one at a time (curried) or all at
-- once (uncurried). Haskell prefers the former.

-- uncurried, i.e, arguments are packaged in a tuple or list
add' :: (Int, Int) -> Int
add' (x, y) = x + y

-- curried, i.e, arguments are processed one at a time
add :: Int -> Int -> Int
add x y = x + y

-- A simple example as to why.
-- Consider adding 1 to every entry of list xs
xs :: [Int]
xs = [1, 2, 3, 4]

-- with curried function: can make use of implicit partial function application
ys :: [Int]
ys = map (add 7) xs

-- with uncurried function: have to specialize our add function
inc::Int -> Int
inc x = add' (x,1)

inc2::Int -> Int
inc2 x = add' (x,2)

zs::[Int]
zs = map inc2 xs

-- Associativity and binding of abstraction (->) and function application.
-- The natural consequence of curried function application is that ->
-- associates to the right: like this.
tripleProduct::Int -> (Int -> (Int -> Int))
tripleProduct x y z = x * y * z


-- And function application associates to the left, like this.
prod = (((tripleProduct 1) 2) 3)

-- With these conventions, we can remove all the brackets. Which makes
-- things a little cleaner to read. Although we should recall the way
-- to read this is as above with the parentheses.


main = do
    print xs
    print ys
    print (tripleProduct 1 2 3)
    print prod

