{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module ParserPaolino where

import Control.Applicative
    ( Alternative (..)
    , Applicative (..)
    , many
    , optional
    , some
    )
import Control.Monad (MonadPlus (..))
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Value (Record, Value)
import Data.Bifunctor (bimap, Bifunctor (bimap))

-- | A parser is a function that may reduce the input and return a value in place
-- of the consumed input.
newtype Parser s a = Parser { runParser :: s -> Maybe (s, a) }

-- | Specific instance over [Char].
type ParserS = Parser String

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser p) = Parser $ fmap (bimap id f) . p

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure x = Parser $ \s -> Just . (, x) $ s
    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    Parser p1 <*> Parser p2 = error "TODO"

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    Parser p >>= f = error "TODO"

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = error "TODO"
    (<|>) :: Parser s a -> Parser s a -> Parser s a
    Parser p1 <|> Parser p2 = error "TODO"

instance (Semigroup a) => Semigroup (Parser s a) where
    (<>) :: Parser s a -> Parser s a -> Parser s a
    (<>) = error "TODO"

instance (Monoid a) => Monoid (Parser s a) where
    mempty :: Parser s a
    mempty = error "TODO"

instance MonadFail (Parser s) where
    fail :: String -> Parser s a
    fail _ = empty -- how to avoid forgetting the error message?

-- historical reasons
instance MonadPlus (Parser s) where
    mzero :: Parser s a
    mzero = empty
    mplus :: Parser s a -> Parser s a -> Parser s a
    mplus = (<|>)

-- take next token from input
consume :: Parser [a] a
consume = error "TODO"

-- observe next token from input without consuming it
peek :: Parser [a] a
peek = error "TODO"

-- match end of input
eof :: Parser [a] ()
eof = error "TODO"

-- match next token of input if it satisfies the predicate
satisfy :: (a -> Bool) -> Parser [a] a
satisfy f = error "TODO"

-- match next token of input if it is equal to the given token
char :: Eq a => a -> Parser [a] a
char c = error "TODO"

-- match a string of tokens
string :: Eq a => [a] -> Parser [a] [a]
string = error "TODO"

-- parse a list of values separated by a given separator
sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy p sep = error "TODO"

-- parse a value enclosed by two ignored parsers
between :: Parser s a -> Parser s b -> Parser s c -> Parser s c
between open close p = error "TODO"

-- parse a value or use a default value if the parser fails
option :: a -> Parser s a -> Parser s a
option x p = error "TODO"

-- parse and skip zero or more space characters
skipSpaces :: ParserS ()
skipSpaces = error "TODO"

-- parse a word as a on-empty string of lowercase letters
word :: ParserS String
word = error "TODO"

-- parse a natural number
natural :: ParserS Int
natural = error "TODO"

-- parse an haskell-style list of values, use between and sepBy
list :: ParserS a -> ParserS [a]
list p = error "TODO"

-- parse a value, use '<|>' to try different parsers
valueP :: ParserS Value
valueP = error "TODO"

-- parse a record
recordP :: ParserS Record
recordP = error "TODO"

-- parse a key-value pair
keyValueP :: ParserS (String, Value)
keyValueP = error "TODO"
