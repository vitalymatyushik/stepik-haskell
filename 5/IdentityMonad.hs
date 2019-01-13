module Identity where
import Control.Monad (ap)

data SomeType a = This a | That a
    deriving Show
instance Monad SomeType where
    return x = This x
    (>>=) (This x) f = f x
instance Applicative SomeType where
    pure  = return
    (<*>) = ap
instance Functor SomeType where
    fmap f x = x >>= return . f