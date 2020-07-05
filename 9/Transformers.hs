{-# LANGUAGE FlexibleContexts #-}
module Transformers where 
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer
import Data.Foldable (traverse_)
import Data.Semigroup
import Text.Read (reads)

data ReadError = EmptyInput | NoParse String
    deriving Show
tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead "" = throwError EmptyInput
tryRead s = check (reads s) where
    check [(n, "")]     = return n
    check [(n, e)]      = throwError (NoParse s)
    check []            = throwError (NoParse s)

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
instance Foldable Tree where
    foldr f ini (Leaf a) = f a ini
    foldr f ini (Fork l x r) = foldr f (f x (foldr f ini r)) l
treeSum :: Tree String -> Either ReadError Integer
treeSum = fmap getSum . execWriterT . traverse_ (tryRead >=> tell . Sum)

limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
runLimited2 p fs s = run2 (limited p fs) s

run1 :: ExceptT Int (State s) [a] -> s -> (Either Int [a], s)
run1 e s = runState (runExceptT e) s
run2 :: StateT s (Except Int) [a] -> s -> Either Int ([a], s)
run2 e s = runExcept (runStateT e s)