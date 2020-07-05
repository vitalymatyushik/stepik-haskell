{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module ImplicitLifting where

import Data.Functor.Identity
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Class
class Functor' c e | c -> e where
    fmap' :: (e -> e) -> c -> c

instance Functor' (Maybe a) a where
    fmap' f (Just a) = Just (f a)
    fmap' f _ = Nothing

instance Functor' [a] a where
    fmap' = map

data Logged a = Logged String a deriving (Eq,Show)
newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }
instance Monad m => Functor (LoggT m) where
    fmap = liftM
instance Monad m => Applicative (LoggT m) where
    pure = return
    (<*>) = ap
instance MonadTrans LoggT where
    lift m = LoggT $ do
        x <- m
        return $ Logged mempty x
instance Monad m => Monad (LoggT m) where
    return a = LoggT $ return (Logged "" a)
    -- fail = LoggT . fail
    m >>= k = LoggT $ do
        (Logged w a) <- runLoggT m
        (Logged w' b) <- runLoggT $ k a
        return $ Logged (w `mappend` w') b
write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s ()
type Logg = LoggT Identity
runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT

instance MonadState s m => MonadState s (LoggT m) where
    get   = lift $ get
    put   = lift . put
    state = lift . state

logSt' :: LoggT (State Integer) Integer      
logSt' = do 
    modify (+1)                   -- no lift!
    a <- get                      -- no lift!
--   write2log $ show $ a * 10
    put 42                        -- no lift!
    return $ a * 100

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT

instance MonadReader r m => MonadReader r (LoggT m) where
    ask    = lift ask
    local f m  = LoggT $ local f (runLoggT m)
    reader = lift . reader

logRdr :: LoggT (Reader [(Int,String)]) ()      
logRdr = do 
    x <- asks $ lookup 2                      -- no lift!
    write2log (maybe "Nothing" id x)
    y <- local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
    write2log (maybe "Nothing" id y)

class Monad m => MonadLogg m where
    w2log :: String -> m ()
    logg :: Logged a -> m a
instance Monad m => MonadLogg (LoggT m) where
    w2log = write2log
    logg  = LoggT . return
instance MonadLogg m => MonadLogg (StateT s m) where
    w2log s = StateT $ \ st  -> do
        w2log s
        return ((), st)
    logg l@(Logged _ st) = StateT $ \ s -> do 
        logg l
        return (st, s)
instance MonadLogg m => MonadLogg (ReaderT r m) where
    w2log s = ReaderT $ \_ -> w2log s
    logg l = ReaderT $ \_ -> logg l
logSt'' :: LoggT (State Integer) Integer      
logSt'' = do 
    x <- logg $ Logged "BEGIN " 1
    modify (+x)
    a <- get
    w2log $ show $ a * 10
    put 42
    w2log " END"
    return $ a * 100
rdrStLog :: ReaderT Integer (StateT Integer Logg) Integer      
rdrStLog = do 
    x <- logg $ Logged "BEGIN " 1
    y <- ask
    modify (+ (x+y))
    a <- get
    w2log $ show $ a * 10
    put 42
    w2log " END"
    return $ a * 100
