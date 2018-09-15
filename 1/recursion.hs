module Recursion where
    factorial n = if n == 0 then 1 else n * factorial (n - 1) 
    
    factorial1 0 = 1
    factorial1 n = n * factorial1 (n - 1)

    doubleFact :: Integer -> Integer
    doubleFact n = if ((n == 0) || (n == 1)) then 1 else n * doubleFact (n-2)


    
    factorial' 0 = 1
    factorial' n = if n < 0 then error "arg must be >= 0" else n * factorial'  (n - 1)

    factorial'' 0 = 1
    factorial'' n | n < 0 = error "arg must be >= 0" 
                  | n > 0 = n * factorial''  (n - 1)


    printOtherwise = print otherwise

    fibonacci' :: Integer -> Integer
    fibonacci' n | n == 0 = 0
                | n == 1 = 1
                | n == -1 = 1
                | n < 0 = (fibonacci' (n + 2)) - (fibonacci' (n + 1))
                | n > 0 = (fibonacci' (n - 1)) + (fibonacci' (n - 2))

    fibonacci :: Integer -> Integer
    fibonacci n | n == 1 = 1
                | n > 0 = (helper 0 0 0 n)
                | n == (-1) = 1
                | n < 0 = (helper2 0 0 0 n)
                | otherwise = 0

    helper :: Integer -> Integer -> Integer -> Integer -> Integer
    helper acc1 acc2 0 n = (helper 0 0 1 n)
    helper acc1 acc2 1 n = (helper 1 0 2 n)
    helper acc1 acc2 k n | k < n = (helper (acc1 + acc2) acc1 (k + 1) n)
                         | k == n = acc1 + acc2

    helper2 :: Integer -> Integer -> Integer -> Integer -> Integer
    helper2 acc1 acc2 0 n = (helper2 0 0 (-1) n)
    helper2 acc1 acc2 (-1) n = (helper2 1 0 (-2) n)
    helper2 acc1 acc2 k n | k > n = (helper2 (acc2 - acc1) acc1 (k - 1) n)
                          | k == n = acc2 - acc1