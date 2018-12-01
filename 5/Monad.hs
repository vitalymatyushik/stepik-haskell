module Monad where
data Log a = Log [String] a deriving Show
toLogger :: (a -> b) -> String -> (a -> Log b)
execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
returnLog :: a -> Log a
bindLog :: Log a -> (a -> Log b) -> Log b
execLoggersList :: a -> [a -> Log a] -> Log a

toLogger f msg = Log [msg] . f
execLoggers a f g = Log (msgb ++ msgc) c where
    (Log msgb b) = f a
    (Log msgc c) = g b
returnLog = Log []
bindLog (Log msga a) f = Log (msga ++ msgb) b where
    (Log msgb b) = f a
execLoggersList = foldl bindLog . returnLog