module Type
    ( Parser (..)
    , ParserS
    )
where

-- a parser is a function that maybe reduce the input and return a vaule in place
-- of the consumed input
newtype Parser s a = Parser {runParser :: s -> Maybe (s, a)}

-- specific instance over [Char]
type ParserS = Parser String
