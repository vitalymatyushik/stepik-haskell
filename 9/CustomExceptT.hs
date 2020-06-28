module CustomExceptT where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Char (isNumber, isPunctuation)
import Data.Foldable (msum, traverse_)
import Data.Semigroup

data Tile = Floor | Chasm | Snake
  deriving Show
data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)
type Point = (Integer, Integer)
type GameMap = Point -> Tile

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gm 0 p = [Right p]
moves gm count (x, y) = runExceptT $ do
    p' <- ExceptT $ [safePoint gm (x, y-1), safePoint gm (x, y+1), safePoint gm (x-1, y), safePoint gm (x+1, y)]
    ExceptT $ moves gm (count - 1) p'

safePoint :: GameMap -> Point -> Either DeathReason Point
safePoint gm p = case gm p of
    Floor -> Right p
    Chasm -> Left Fallen
    Snake -> Left Poisoned

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r gm count p = length $ filter isDead (moves gm count p) where
    isDead (Left r') | r == r' = True
                     | otherwise = False
    isDead _ = False

map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

data PwdError = PwdError String
type PwdErrorIOMonad = ExceptT PwdError IO
instance Semigroup PwdError where
    PwdError x <> PwdError y = PwdError (x `mappend` y)
instance Monoid PwdError where
    mempty = PwdError ""
    (PwdError x) `mappend` (PwdError y) = PwdError $ x `mappend` y

askPassword :: PwdErrorIOMonad ()
askPassword = do
    liftIO $ putStrLn "Enter you new password:"
    value <- msum $ repeat getValidPassword
    liftIO $ putStrLn "Storing in database..."
  
getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
    s <- liftIO getLine
    catchE (isValid s) logError

logError :: PwdError -> PwdErrorIOMonad String
logError x@(PwdError e) = do
    liftIO $ putStrLn e
    throwE x

isValid :: String -> PwdErrorIOMonad String
isValid s | length s <= 8 = throwE $ PwdError "Incorrect input: password is too short!"
          | not (any isNumber s) = throwE $ PwdError "Incorrect input: password must contain some digits!"
          | not (any isPunctuation s) = throwE $ PwdError "Incorrect input: password must contain some punctuation!"
          | otherwise = return s

data ReadError = EmptyInput | NoParse String
  deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead "" = throwE EmptyInput
tryRead s = check (reads s) where
    check [(n, "")]     = return n
    check [(n, e)]      = throwE (NoParse s)
    check []            = throwE (NoParse s)

data Tree a = Leaf a | Fork (Tree a) a (Tree a)
instance Foldable Tree where
    foldr f ini (Leaf a) = f a ini
    foldr f ini (Fork l x r) = foldr f (f x (foldr f ini r)) l
go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go s = tryRead s >>= (lift . tell . Sum)
treeSum t = let (err, s) = runWriter . runExceptT $ traverse_ go t
            in (maybeErr err, getSum s)
  where
    maybeErr :: Either ReadError () -> Maybe ReadError
    maybeErr = either Just (const Nothing)