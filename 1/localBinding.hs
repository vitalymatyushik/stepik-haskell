module Root where

roots a b c =
    let d = sqrt (b ^ 2 - 4 * a * c) in
        ((-b - d) / (2 * a), (-b + d) / (2 * a))

seqA :: Integer -> Integer
seqA n 
    | n > 2 = let
        helper first second third 3 = third + second - 2 * first 
        helper first second third counter = helper second third (third + second - 2 * first)  (counter - 1)
      in helper 1 2 3 n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x >= 0 = ((sum 0 x), (count 0 x))
              | x == 0 = (0, 1) 
              | otherwise = ((sum 0 (-x)), (count 0 (-x)))
    where 
        sum :: Integer -> Integer -> Integer
        sum acc k | k < 10 = acc + k
                  | otherwise = sum ((mod k 10) +  acc) (div k 10)
        count :: Integer -> Integer -> Integer
        count acc k | k < 10 = acc + 1
                    | otherwise = count (acc + 1) (div k 10)
    
integration :: (Double -> Double) -> Double -> Double -> Double

    