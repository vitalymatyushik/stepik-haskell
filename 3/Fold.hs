module Fold where
    
meanList :: [Double] -> Double
meanList = divide . foldr (\x (s,m) -> (s + x, m + 1)) (0,0) where
    divide :: (Double, Double) -> Double
    divide (a,b) = a / b

evenOnly' :: [a] -> [a]
evenOnly' = fst . foldl (\(xs,i) x -> if odd i then (xs ++ [x], i + 1) else (xs, i + 1)) ([], 0)

evenOnly :: [a] -> [a]
evenOnly = snd . foldr (\x ~(xs, ys) -> (x : ys, xs)) ([], [])