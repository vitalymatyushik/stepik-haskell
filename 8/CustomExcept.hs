module CustomExcept where
import Control.Monad (liftM, ap, MonadPlus(mzero, mplus), guard, msum)
import Control.Applicative (Alternative(empty, (<|>)))

newtype Except e a = Except {runExcept :: Either e a}
                        deriving Show
except :: Either e a -> Except e a
except = Except

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept _ (Except (Right a)) = except $ Right a
withExcept f (Except (Left e)) = except $ Left (f e)

instance Functor (Except e) where
    fmap = liftM
instance Applicative (Except e) where
    pure = return
    (<*>) = ap
instance Monad (Except e) where
    return a = Except (Right a)
    m >>= k = case runExcept m of
                Left e -> Except (Left e)
                Right x -> k x