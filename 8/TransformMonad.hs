module TransformMonad where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Char (toUpper)

logFirstAndRetSecond' :: WriterT String (Reader [String]) String
logFirstAndRetSecond' = do
    el1 <- lift $ asks head
    el2 <- lift $ asks (map toUpper . head . tail)
    tell el1
    return el2

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate pr1 pr2 xs = do
    let l1 = filter pr1 xs
    let l2 = filter pr2 xs
    let l3 = filter (\x -> (not ((pr1 x) || (pr2 x)))) xs
    tell l1
    lift $ tell l2
    return l3

type MyRW = ReaderT [String] (Writer String)
runMyRW' :: MyRW a -> [String] -> (a, String)
runMyRW' rw e = runWriter (runReaderT rw e)

myAsks' :: ([String] -> a) -> MyRW a
myAsks' = asks

myTell' :: String -> MyRW ()
myTell' = lift . tell

logFirstAndRetSecond'' :: MyRW String
logFirstAndRetSecond'' = do
  el1 <- myAsks' head
  el2 <- myAsks' (map toUpper . head . tail)
  myTell' el1
  return el2

type MyRWT m =  ReaderT [String] (WriterT String m)
runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt = runWriterT . runReaderT rwt

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell ::  Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

logFirstAndRetSecond''' :: MyRWT Maybe String
logFirstAndRetSecond''' = do
    xs <- myAsks id
    case xs of
        (el1 : el2 : _) -> myTell el1 >> return (map toUpper el2)
        _ -> myLift Nothing

veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
    exs <- myAsks (filter $ even . length)
    oxs <- myAsks (filter (odd . length))
    case exs of
        (eel1 : eel2 : _) -> 
            case oxs of 
                (oel1: oel2: _) -> myTell (eel1 ++ "," ++ oel1) >> return (map toUpper eel2, map toUpper oel2)
        _ -> myLift Nothing


type EsSi = ExceptT String (State Integer)
tickCollatz :: State Integer Integer
tickCollatz = do
    n <- get
    let res = if odd n then 3 * n + 1 else n `div` 2
    put res
    return n

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi = runState . runExceptT

go' :: Integer -> Integer -> State Integer Integer -> EsSi ()
go' l u st = do
    n <- lift get
    let next = execState st n
    lift $ put next
    when (next >= u) (throwE "Upper bound")
    when (next <= l) (throwE "Lower bound")


type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))
tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
    n <- get
    let res = if odd n then 3 * n + 1 else n `div` 2
    lift $ putStrLn $ show res
    put res
    return n
runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                 -> (Integer,Integer) 
                 -> Integer 
                 -> m (Either String a, Integer)
runRiiEsSiT r e = runStateT (runExceptT (runReaderT r e))

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go st =  do
    (l, u) <- ask
    lift $ lift st
    n <- lift (lift get)
    when (n >= u) (lift $ throwE "Upper bound")
    when (n <= l) (lift $ throwE "Lower bound")