module Bit where

import Data.List (unfoldr)

data Bit = Zero | One
data Sign = Minus | Plus
data Z = Z Sign [Bit]

zToInt (Z Minus bits) = - zToInt (Z Plus bits)
zToInt (Z Plus bits) = foldr (\x a -> a * 2 + toInt x) 0 bits where
  toInt One = 1
  toInt Zero = 0

intToZ x = (Z (sign x) (toBin $ abs x)) where
    sign n = if n < 0 then Minus else Plus
    toDigit 0 = Zero
    toDigit 1 = One
    toBin = unfoldr (\x -> if x > 0
                        then Just (toDigit $ rem x 2, div x 2)
                        else Nothing)


add :: Z -> Z -> Z
add a b = intToZ $ (zToInt a) + (zToInt b)
  
mul :: Z -> Z -> Z
mul a b = intToZ $ (zToInt a) * (zToInt b)