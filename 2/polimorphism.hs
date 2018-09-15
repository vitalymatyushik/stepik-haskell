module Polimorphism where
    
    import Data.Function

    getSecondFrom :: t1 -> t2 -> t3 -> t2
    getSecondFrom x y z = y

    multSecond :: (a, Integer) -> (a, Integer) -> Integer
    multSecond = (*) `on` snd

    on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
    on3 op f x y z = (op (f x) (f y) (f z))

    doItYourself = f . g . h
    h x = (max 42 x)
    g y = y ^ 3
    f = logBase 2

    swap :: (a,b) -> a
    swap = uncurry (const)

    -- class Eql a where
    --     (==) :: a -> a -> Bool
    --     (/=) :: a -> a -> Bool
    
    -- instance Eql Bool where
    --     True == True = True
    --     False == False = False

    --     _ == _ = False

    --     x /= y = not (x Polimorphism.== y)
    
    class Printable a where
        toString :: a -> String

    instance Printable Bool where
        toString True  = "true"
        toString False = "false"

    instance Printable () where
        toString () = "unit type"

    instance (Printable a, Printable b) => Printable(a,b) where
        toString p = "("++(toString(fst p))++","++(toString(snd p))++")"

    class KnownToGork a where
        stomp :: a -> a
        doesEnrageGork :: a -> Bool
    
    class KnownToMork a where
        stab :: a -> a
        doesEnrageMork :: a -> Bool
    
    class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
        stompOrStab :: a -> a
        stompOrStab a = f1 a where
            f1 :: (KnownToGork a, KnownToMork a) => a -> a
            f1 x | (doesEnrageGork x) && (doesEnrageMork x) = stomp (stab x)
                 | (doesEnrageGork x) = stab x
                 | (doesEnrageMork x) = stomp x
                 | otherwise = x
    
    a = 127.22
    b = 4.12
    c = 0.1
    d = 2

    ip = show a ++ show b ++ show c ++ show d

    class (Enum a, Bounded a) => SafeEnum a where
        ssucc :: a -> a
        spred :: a -> a

        -- ssucc a | (fromEnum a :: Int) < (fromEnum maxBound "" = succ a
        --         | otherwise = minBound
        -- spred a | fromEnum a > fromEnum minBound = pred a
        --         | otherwise = maxBound

        isSingle :: [a] -> Bool
        isSingle [a] = True
        isSingle _ = False
                
        isLastElement :: Enum a => a -> Bool
        isLastElement a = isSingle $ enumFrom a
                
        isFirstElement :: (Enum a, Bounded a) => a -> Bool
        isFirstElement a = isSingle $ enumFromTo minBound a
                
                
        spred x = if isFirstElement x then maxBound else pred x        
        ssucc x = if isLastElement x then minBound else succ x

    avg :: Int -> Int -> Int -> Double
    avg a b c = (fromRational(toRational (toInteger a + toInteger b + toInteger c)) / 3)
     