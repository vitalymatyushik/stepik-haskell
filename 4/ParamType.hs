module LogEntry where
import Data.Char(isDigit)
import Data.List(find)
data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = (sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2))

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

getCenter :: Double -> Coord Int -> Coord Double
getCenter s (Coord x y) = Coord (coor x) (coor y) where
    coor z = s * (fromIntegral z + 0.5)

getCell :: Double -> Coord Double -> Coord Int
getCell s (Coord x y) = Coord (coor x) (coor y) where
    coor z = floor $ z / s

findDigit :: [Char] -> Maybe Char
findDigit = find isDigit

findDigitOrX :: [Char] -> Char
findDigitOrX str = case findDigit str of Nothing -> 'X'
                                         Just c -> c 

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing