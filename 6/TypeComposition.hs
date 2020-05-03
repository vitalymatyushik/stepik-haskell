{-# LANGUAGE TypeOperators #-}
module TypeComposition where

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)}
    deriving (Eq, Show)

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (1, ('a', True))

b :: B t
b = Cmps (True, id, Right 2)

c :: C
c  = Cmps (\bool a -> 2)

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap j (Cmps3 x) = Cmps3 $ fmap (fmap (fmap j)) x

instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x
instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    Cmps h <*> Cmps x = Cmps $ fmap (<*>) h <*> x
unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 (Cmps x) = fmap getCmps x
unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 (Cmps x) = fmap unCmps3 x