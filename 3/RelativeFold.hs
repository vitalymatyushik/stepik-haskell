module RelativeFold where
    import Data.List (unfoldr)
    import Data.Maybe
    
    lastElem :: [a] -> a    
    lastElem = foldl1 (\x y -> y)

    revRange :: (Char,Char) -> [Char]
    revRange = unfoldr g where 
        g (a,b) | a > b = Nothing
                | otherwise = Just (b, (a, pred b))