module Parser where
import Control.Applicative
import Data.Char
newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
    fmap f p = Prs fun where
        fun s = case runPrs p s of
            Nothing -> Nothing
            Just (a, s') -> Just (f a, s')

anyChr :: Prs Char
anyChr = Prs f where
    f [] = Nothing
    f (c:cs) = Just (c, cs)
char :: Char -> Prs Char
char a  = Prs f where
    f [] = Nothing
    f (c:cs) | a == c = Just (c, cs)
             | otherwise = Nothing

instance Applicative Prs where
    pure a  = Prs fun where
        fun s = Just (a, s)
    pf <*> pv = Prs fun where
        fun s = case runPrs pf s of
                Nothing -> Nothing
                Just (g, s') -> case runPrs pv s' of
                    Nothing -> Nothing
                    Just (a, s'') -> Just (g a, s'')

instance Alternative Prs where
    empty = Prs f where
        f _ = Nothing
    p <|> q = Prs f where
        f s =  case runPrs p s of
                    Nothing         -> runPrs q s
                    Just (a, s')    -> Just (a, s')
many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> (many p <|> pure [])
nat :: Prs Int
nat = read <$> many1 digit where
    digit = satisfy isDigit where
        satisfy pr = Prs f where
            f "" = Nothing
            f (c:cs) | pr c = Just (c,cs)
                     | otherwise = Nothing
mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

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