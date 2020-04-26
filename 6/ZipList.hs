import Control.Applicative (ZipList(ZipList), getZipList)

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) = \xs ys -> getZipList $ ZipList xs <*> ZipList ys