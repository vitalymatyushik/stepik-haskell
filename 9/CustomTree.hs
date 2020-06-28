module CustomTree where
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Monoid
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Eq, Show)

go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go (Leaf _) = do
    i <- get
    put (i + 1)
    lift $ tell 1
    return $ Leaf i
go (Fork left _ right) = do
    l <- go left
    i <- get
    put (i + 1)
    r <- go right
    return $ Fork l i r

numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)