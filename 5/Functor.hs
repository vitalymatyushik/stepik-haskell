module Functor where
data Point3D a = Point3D a a a deriving Show
instance Functor Point3D where
    fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)
instance Functor GeomPrimitive where
    fmap f (Point a) = Point $ (fmap f a)
    fmap f (LineSegment a b) = LineSegment (fmap f a) (fmap f b)

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show
instance Functor Tree where
    fmap f (Leaf a) = Leaf (fmap f a)
    fmap f (Branch l a r) = Branch (fmap f l) (fmap f a) (fmap f r)

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show
instance Functor (Map k1 k2) where
    fmap f (Map entries) = Map (map (fmap f) entries)