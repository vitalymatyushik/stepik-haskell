module CustomFoldable where
import Data.Monoid
data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where 
    foldr f ini (Tr a b c) = f a (f b (f c ini))
    foldl f ini (Tr a b c) = f (f (f ini a) b) c

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Foldable Preorder where
    foldr f ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
    foldr f ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
    foldr f ini (LevelO Nil) = ini
    foldr f ini (LevelO treeLevel) = let level [] = ini 
                                         level (Nil:xs) = level xs
                                         level ((Branch l x r):xs) = f x (level (xs++[l,r]))
                                     in level [treeLevel]

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldr (mappend . Endo) (Endo id) 

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 
instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldr h ini (Cmps x)  = foldr (flip $ foldr h) ini x