{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module CustomCoroutine where
-- Пожалуйста, не удаляйте эти импорты. Они нужны для тестирующей системы.
import Control.Monad (ap, liftM)
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable

newtype CoroutineT m a = CoroutineT { runCoroutineT :: m (Either (CoroutineT m a) a) }
instance Monad m => Functor (CoroutineT m) where
    fmap = liftM
instance Monad m => Applicative (CoroutineT m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (CoroutineT m) where
    return = CoroutineT . return . Right
    (CoroutineT ma) >>= k = CoroutineT $ do
        ma' <- ma
        case ma' of
            Right a -> runCoroutineT $ k a
            Left ca -> return $ Left $ ca >>= k
instance MonadTrans CoroutineT where
    lift ma = CoroutineT $ Right <$> ma
instance MonadWriter w m => MonadWriter w (CoroutineT m) where
    tell = lift . tell
    listen = undefined
    pass = undefined

runCoroutines :: Monad m => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines ca cb = do
    ca' <- runCoroutineT ca
    case ca' of
        Right _ -> runCoroutine cb
        Left ca'' -> runCoroutines cb ca''

runCoroutine :: Monad m => CoroutineT m () -> m ()
runCoroutine c = do
    c' <- runCoroutineT c
    case c' of
        Right _ -> return ()
        Left c'' -> runCoroutine c''

yield :: Monad m => CoroutineT m ()
yield = CoroutineT $ return $ Left $ return ()

coroutine1 :: CoroutineT (Writer String) ()
coroutine1 = do
    tell "1"
    yield
    tell "2"

coroutine2 :: CoroutineT (Writer String) ()
coroutine2 = do
    tell "a"
    yield
    tell "b"