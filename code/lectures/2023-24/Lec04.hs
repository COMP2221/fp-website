-- all factors of n
factors:: Int -> [Int]
factors n = [x|x <- [1..n], n `mod` x == 0]

-- is prime
isPrime:: Int -> Bool
isPrime n = factors n == [1,n]

-- all primes <= n
primes:: Int -> [Int]
primes n = [x| x <- [2..n], isPrime x]



main = do
    print "Hello, world!"
    print (factors 10)
    print (isPrime 1)
    print (primes 7)
