    module Hof where
    import Data.Char (isDigit)

    readDigits :: String -> (String, String)
    readDigits = span isDigit

    filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
    filterDisj f1 f2 as = filter (orFilter f1 f2) as where
            orFilter :: (a -> Bool) -> (a -> Bool) -> a -> Bool
            orFilter f1 f2 a | f1 a = True
                             | otherwise = f2 a

    qsort :: Ord a => [a] -> [a]
    qsort [] = []
    qsort (x:[]) = [x]
    qsort l@(x:xs) = (qsort (filter (< x) xs)) ++ (filter (== x) l) ++ (qsort (filter (> x) xs))

    squares'n'cubes :: Num a => [a] -> [a]
    squares'n'cubes = concatMap (\x -> [x * x] ++ [x * x * x])

    perms :: [a] -> [[a]]
    perms [] = [[]]
    perms (a:[]) = [[a]]
    perms (a:b:[]) = [[a,b],[b,a]]
    perms (a:as) = (concatMap (\xs -> insertAtAll a xs) (perms as)) where
        insertAtAll :: c -> [c] -> [[c]]
        insertAtAll c [] = [[c]]
        insertAtAll c (b:[]) = [[c,b],[b,c]]
        insertAtAll c bl@(b:bs) = [[c] ++ bl] ++ (map (\xs -> [b] ++ xs) (insertAtAll c bs))

    delAllUpper :: String -> String
    delAllUpper a = unwords . filter (\xs -> any (\x -> x < 'A' || x > 'Z') xs) $ (words a)

    max3 :: Ord a => [a] -> [a] -> [a] -> [a]
    max3 a b c = zipWith (max) c (zipWith (max) a b)