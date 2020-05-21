module MonadAndApplicative where
newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }
satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
    f ""                    = Left "unexpected end of input"
    f (c:cs) | pr c         = Right (c,cs)
             | otherwise    = Left ("unexpected " ++ [c])
charE :: Char -> PrsE Char
charE c = satisfyE (== c)
instance Functor PrsE where
    fmap f p = PrsE fun where
        fun s = case runPrsE p s of
            Left s'             -> Left s'
            Right (a, s')       -> Right (f a, s')

instance Applicative PrsE where
    pure a  = PrsE fun where
        fun s = Right (a, s)
    pf <*> pv = PrsE fun where
        fun s = case runPrsE pf s of
                Left s'         -> Left s'
                Right (g, s') -> case runPrsE pv s' of
                    Left s''        -> Left s''
                    Right (a, s'')  -> Right (g a, s'')
instance Monad PrsE where
    (PrsE v) >>= f = PrsE fun where
        fun s = do {(a, s') <- v s; runPrsE (f a) s'}

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) c = Bi a b c
concat3OC (Un a) (Bi x y z) c = Bi a x (concat3OC (Un y) z c)
concat3OC (Bi x y z) w c = Bi x y (concat3OC z w c)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi a b c) = concat3OC a b (concatOC c)

instance Functor OddC where
    fmap f (Un a) = Un (f a)
    fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)
instance Monad OddC where
    (Un a) >>= f = f a
    (Bi a b c) >>= f = concat3OC (f a) (f b) (c >>= f)
instance Applicative OddC where
    pure a = Un a 
    (odf) <*> (oda) = do {f<-odf; a<-oda; return (f a)}
