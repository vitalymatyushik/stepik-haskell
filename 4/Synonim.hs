module Synonim where
import Data.Monoid
import Prelude hiding (lookup)
import qualified Data.List as L

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    Xor a `mappend` Xor b = Xor (a /= b)

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    (Maybe' Nothing) `mappend` _ = Maybe' Nothing
    _ `mappend` (Maybe' Nothing) = Maybe' Nothing
    (Maybe' a) `mappend` (Maybe' b) = Maybe' (mappend a b)

-- class MapLike m where
--     empty :: m k v
--     lookup :: Ord k => k -> m k v -> Maybe v
--     insert :: Ord k => k -> v -> m k v -> m k v
--     delete :: Ord k => k -> m k v -> m k v
--     fromList :: Ord k => [(k,v)] -> m k v
--     fromList [] = empty
--     fromList ((k,v):xs) = insert k v (fromList xs)

-- newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
--     deriving (Eq,Show)

-- instance MapLike ListMap where
--     empty = ListMap []
--     lookup key (ListMap list) = List.lookup key list
--     insert key value map  = ListMap $ (key, value) : (getListMap $ delete key map)
--     delete key (ListMap list)  = ListMap $ filter (\ (x, _) -> x /= key) list

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (const Nothing)
    lookup key (ArrowMap map) = map key
    insert k v (ArrowMap f) = ArrowMap $ \x -> if x == k then return v else f x
    delete k (ArrowMap f) = ArrowMap $ \x -> if x == k then Nothing else f x
    fromList = foldr (\kv m -> insert (fst kv) (snd kv) m) empty