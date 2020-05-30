module CustomCont where
import Control.Monad (liftM, ap)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except

decode c = c $ 0 
as x c = c $ x
a x c = c $ x
number = id

one x c = c $ (x + 1)
two x c = c $ (x + 2)
three x c = c $ (x + 3)
seventeen x c = c $ (x + 17)
twenty x c = c $ (x + 20)
hundred x c = c $ (x * 100)
thousand x c = c $ (x * 1000)

showCont :: Show a => Cont String a -> String
showCont c = runCont c show

type Checkpointed a = (a -> Cont a a) -> Cont a a
addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}
runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed p cp = runCont (cp checkpoint) id where
                        checkpoint x = do 
                            val <- cont (\c -> if p (c x) then c x else x)
                            return $ val

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }
instance Functor (FailCont r e) where
    fmap = liftM
instance Applicative (FailCont r e) where
    pure = return
    (<*>) = ap
instance Monad (FailCont r e) where 
    return a = FailCont (\ok _ -> ok a)
    FailCont v >>= k = FailCont $ \ok e -> v (\a -> runFailCont (k a) ok e) e

toFailCont :: Except e a -> FailCont r e a
toFailCont e = case runExcept e of 
                Left e  -> FailCont (\_ err -> err e)
                Right a -> FailCont (\ok _ -> ok a)

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont c = runFailCont c Right Left

data ReadError = EmptyInput | NoParse String
    deriving Show
tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE EmptyInput
tryRead s = check (reads s) where
    check [(n, "")]     = return n
    check [(n, e)]      = throwE (NoParse s)
    check []            = throwE (NoParse s)

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

addInts :: String -> String -> FailCont r ReadError Int
addInts s1 s2 = do
  i1 <- toFailCont $ tryRead s1
  i2 <- toFailCont $ tryRead s2
  return $ i1 + i2

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \ok err -> runFailCont (f $ \a -> FailCont $ \_ _ -> ok a) ok err

ex2 = do
    a <- return 1
    b <- cont (\fred -> fred 10)
    return $ a+b

test2 = runCont ex2 show

main = print test2