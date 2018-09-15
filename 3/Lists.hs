module Lists where
    import Data.List (take, iterate)
    addTwoElements :: a -> a -> [a] -> [a]
    addTwoElements a b xs = a : b : xs

    nTimes :: a -> Int -> [a]
    nTimes x n =  take n $ repeat (x)

    sndHead ((:) ((,) _ x) y) = x

    oddsOnly :: Integral a => [a] -> [a]
    oddsOnly xs = filter odd xs

    isPalindrome :: Eq a => [a] -> Bool
    isPalindrome x = x == reverse x

    sum3 :: Num a => [a] -> [a] -> [a] -> [a]
    sum3 [] [] [] = []
    sum3 as [] [] = as
    sum3 [] bs [] = bs
    sum3 [] [] cs = cs
    sum3 (a:as) (b:bs) [] = (a + b) : sum3 as bs []
    sum3 (a:as) [] (c:cs) = (a + c) : sum3 as [] cs
    sum3 [] (b:bs) (c:cs) = (b + c) : sum3 [] bs cs     
    sum3 (a:as) (b:bs) (c:cs) =  (a + b + c) : sum3 as bs cs

    groupElems :: Eq a => [a] -> [[a]]
    groupElems [] = []
    groupElems (x:[]) = [[x]]
    groupElems (x:xs) = [(takeWhile (== x) xs) ++ [x]] ++ groupElems (dropWhile (== x) xs)