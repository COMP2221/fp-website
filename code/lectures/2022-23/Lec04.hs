factors :: Int -> [Int]
factors n = [x | x <-[1..n], n `mod` x == 0]

prime :: Int -> Bool
prime x = factors x == [1,x]

primes :: Int -> [Int]
primes n = [x|x <- [1..n], prime x]

--define Pair as synonym for a tuple of two elements with the same type
type Pair a = (a,a)
type Triple a = (a,a,a)

sumpair::Pair Int -> Int
sumpair (x,y) = x+y

dup:: Int -> Pair Int
dup x = (x,x)

data Nat = Zero | Succ Nat
    deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

data MyList a = Empty | Cons a (MyList a)
    deriving (Show)

intList :: MyList Integer
intList = Cons 1 (Cons 2 (Cons 3 Empty))


main = do
    print (add Zero (Succ (Succ Zero)))
    print (factors 10)
    print (prime 7)
    print (primes 7)