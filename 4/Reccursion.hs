module Reccursion where

data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = a : fromList b

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x $ toList xs

data Nat = Zero | Suc Nat deriving Show
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero x = x
add (Suc x) y = add x (Suc y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc x) y = add (mul x y) y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac n1@(Suc n) = mul n1 (fac n)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + max (height a) (height b) 

size :: Tree a -> Int
size (Leaf _) = 1
size (Node a b) = 1 + (size a) + (size b)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node l r) = let
        (lc, ls) = go l
        (rc, rs) = go r
      in (lc + rc, ls + rs)


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (expand e1 :*: expand e) :+: expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = expand (expand e :*: expand e1) :+: expand (expand e :*: expand e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = let
    ee1 = expand e1
    ee2 = expand e2
  in if ee1 == e1 && ee2 == e2 then e1 :*: e2 else expand $ ee1 :*: ee2
expand e = e