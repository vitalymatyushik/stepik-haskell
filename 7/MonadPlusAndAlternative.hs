module MonadPlusAndAlternative where
import Control.Applicative
newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP f where
    f pos ""                    = (succ pos, Left ("pos " ++ show (succ pos) ++ ": unexpected end of input"))
    f pos (c:cs) | pr c         = (succ pos, Right (c,cs))
            | otherwise         = (succ pos, Left ("pos " ++ show (succ pos) ++ (": unexpected " ++ [c])))
instance Functor PrsEP where
    fmap f p = PrsEP fun where
        fun pos s = case runPrsEP p pos s of
            (pos', Left s')         -> (pos', Left s')
            (pos', Right (a, s'))   -> (pos', Right (f a, s'))

instance Applicative PrsEP where
    pure a  = PrsEP fun where
        fun pos s = (pos, Right (a, s))
    pf <*> pv = PrsEP fun where
        fun pos s = case runPrsEP pf pos s of
                    (pos', Left s')         -> (pos', Left s')
                    (pos', Right (g, s'))   -> case runPrsEP pv pos' s' of
                                                (pos'', Left s'')           -> (pos'', Left s'')
                                                (pos'', Right (a, s''))     -> (pos'', Right (g a, s''))
-- charEP c = satisfyEP (== c)
-- anyEP = satisfyEP (const True)
-- testP = (,) <$> anyEP <* charEP 'B' <*> anyEP

instance Alternative PrsEP where
    empty   = PrsEP f where 
                f pos _ = (pos, Left ("pos " ++ show pos ++ ": empty alternative"))
    p <|> q = PrsEP f where
                f pos s = deepest (runPrsEP p pos s) (runPrsEP q pos s) where
                            deepest r@(_, Right _) (_, _)       = r
                            deepest (_, Left _) l@(_, Right _)  = l
                            deepest r@(pos', Left _) l@(pos'', Left _)  | pos' > pos'' = r 
                                                                        | otherwise = l