module StandardExcept where
import Control.Monad.Trans.Except

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
    deriving (Eq, Show)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) a i = check a i where 
    check [] _      = throwE $ ErrIndexTooLarge i
    check (x:_) 0   = return x
    check (x:xs) j | j < 0  = throwE ErrNegativeIndex
                   | otherwise = check xs (j-1)

data ReadError = EmptyInput | NoParse String
    deriving Show
tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = check (reads s) where
    check [(n, "")]     = return n
    check [(n, e)]      = throwE (NoParse s)
    check []            = throwE (NoParse s)

data SumError = SumError Int ReadError
    deriving Show
trySum :: [String] -> Except SumError Integer
trySum = fmap sum . traverse convertOne . zip [1..]
convertOne :: (Int, String) -> Except SumError Integer
convertOne (n, s) = withExcept (SumError n) (tryRead s)

newtype SimpleError = Simple { getSimple :: String } 
    deriving (Eq, Show)
instance Semigroup SimpleError where
    Simple m1 <> Simple m2 = Simple (m1 ++ m2)
instance Monoid SimpleError where
    mempty = Simple mempty
    (Simple x) `mappend` (Simple y) = Simple (x `mappend` y)

lie2se :: ListIndexError -> SimpleError
lie2se (ErrIndexTooLarge ind) = Simple $ "[index (" ++ show ind ++ ") is too large]"
lie2se (ErrNegativeIndex) = Simple $ "[negative index]"

newtype Validate e a = Validate { getValidate :: Either [e] a }
instance Functor (Validate e) where
  fmap = (<*>) . pure
instance Applicative (Validate e) where
  pure = Validate . Right
  vf <*> va = check (getValidate vf) (getValidate va) where
    check (Left ef) (Left ev) = Validate $ Left $ ef ++ ev
    check fun a              = Validate $ fun <*> a
collectE :: Except e a -> Validate e a
collectE e = case runExcept e of 
                Left e  -> Validate (Left [e])
                Right a -> Validate (Right a)
validateSum :: [String] -> Validate SumError Integer
validateSum = fmap sum . traverse (collectE . convertOne) . zip [1..]