{-# LANGUAGE TypeOperators #-}
module CustomTraversable where
traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\x y -> pure (:) <*> (f x) <*> y) (pure [])

data Triple a = Tr a a a  deriving (Eq,Show)
instance Functor Triple where
    fmap f (Tr a b c) = Tr (f a) (f b) (f c)
instance Foldable Triple where 
    foldr f ini (Tr a b c) = f a (f b (f c ini))
    foldl f ini (Tr a b c) = f (f (f ini a) b) c
instance Traversable Triple where
    traverse f (Tr a b c) = pure Tr <*> (f a) <*> (f b) <*> (f c) 

data Result a = Ok a | Error String deriving (Eq,Show)
instance Functor Result where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Error b) = Error b
instance Foldable Result where
  foldr f ini (Ok a) = f a ini
  foldr _ ini (Error b) = ini
instance Traversable Result where
    traverse f (Ok a) = pure Ok <*> f a
    traverse f (Error b) = pure (Error b)

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)
instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l
instance Traversable Tree where
    traverse f Nil = pure Nil
    traverse f (Branch l x r) = Branch <$> (traverse f l) <*> (f x) <*> (traverse f r)

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 
instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x
instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldr h ini (Cmps x)  = foldr (flip $ foldr h) ini x
instance (Traversable f, Traversable g) => Traversable (f |.| g) where
    traverse f (Cmps t) = Cmps <$> traverse (traverse f) t