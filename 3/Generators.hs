module Generators where
    
fibStream :: [Integer]
fibStream = [0, 1] ++ zipWith (+) fibStream (tail fibStream)

repeat = iterate repeatHelper where
    repeatHelper = id

data Odd = Odd Integer 
    deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                      | otherwise      = error "addEven: second parameter cannot be odd"

instance Enum Odd where
    succ (Odd a) = Odd (a + 2)
    pred (Odd a) = Odd (a - 2)
    toEnum = Odd . toInteger
    fromEnum (Odd a) = fromIntegral a
    enumFrom (Odd a) = Odd a : (enumFrom . succ . Odd $ a)
    enumFromThen (Odd a) (Odd b) = (Odd a) : enumFromThen (Odd b) (Odd (2 * b - a))
    enumFromTo (Odd a) (Odd b)
        | a > b = []
        | (Odd a) == (Odd b) = [(Odd a)]
        | otherwise = (Odd a) : enumFromTo (succ $ Odd $ a) (Odd b)

    enumFromThenTo (Odd a) (Odd b) (Odd c)
        | a == b && a == c = Odd a : enumFromThenTo (Odd a) (Odd b) (Odd c)
        | a == b = []
        | (a < b && a > c) || (a > b && a < c) = []
        | otherwise = (Odd a) : enumFromThenTo (Odd b) (Odd (b + step)) (Odd c) where step = b - a

coins :: (Ord a, Num a) => [a]
coins = [2, 3, 7]
change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change a = [x:xs | x <- coins, x <= a, xs <- (change (a - x))]