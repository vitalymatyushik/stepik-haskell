module Maybe where
    import Data.Char (isDigit)

    data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
        deriving (Eq, Show)

    asToken :: String -> Maybe Token
    asToken "(" = Just LeftBrace
    asToken ")" = Just RightBrace
    asToken "+" = Just Plus
    asToken "-" = Just Minus
    asToken param = if all isDigit param then Just (Number (read param::Int)) else Nothing

    tokenize :: String -> Maybe [Token]
    tokenize = sequence . (map asToken) . words