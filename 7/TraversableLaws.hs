{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TraversableLaws where
import Data.Traversable (foldMapDefault)
data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
instance Functor OddC where
    fmap f (Un a) = Un (f a)
    fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)
instance Foldable OddC where 
    foldr f ini (Un a) = f a ini
    foldr f ini (Bi a b c) = f a (f b (foldr f ini c))
instance Traversable OddC where
    traverse f (Un a) = pure Un <*> (f a)
    traverse f (Bi a b c) = pure Bi <*> (f a) <*> (f b) <*> (traverse f c)

newtype Temperature a = Temperature Double
  deriving (Num, Show, Fractional, Eq)

data Celsius
data Fahrenheit 
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature (k - 273.15)

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)
instance Foldable Tree where
    foldMap = foldMapDefault
instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)
instance Traversable Tree where
    traverse f Nil = pure Nil
    traverse f (Branch l x r) = branch <$> traverse f l <*> traverse f r <*> f x where
                                    branch left right a = Branch left a right