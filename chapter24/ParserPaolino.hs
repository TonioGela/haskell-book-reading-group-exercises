{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module ParserPaolino () where

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
import Data.Char

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
    Parser p1 <*> Parser p2 = Parser $ \s -> do
      (s', f) <- p1 s
      (s'', a) <- p2 s'
      return (s'', f a)

instance Monad (Parser s) where
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    Parser p >>= f = Parser $ \s -> do
      (s', a) <- p s
      runParser (f a) s'

-- Use alternative for Maybe
instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ \s -> Nothing
    (<|>) :: Parser s a -> Parser s a -> Parser s a
    Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

instance (Semigroup a) => Semigroup (Parser s a) where
    (<>) :: Parser s a -> Parser s a -> Parser s a
    (<>) p1 p2 = (<>) <$> p1 <*> p2

instance (Monoid a) => Monoid (Parser s a) where
    mempty :: Parser s a
    mempty = pure mempty

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
consume = Parser $ \case
    [] -> Nothing
    (x : xs) -> Just (xs, x)

-- observe next token from input without consuming it
peek :: Parser [a] a
peek = Parser $ \case
    [] -> Nothing
    (x : xs) -> Just (x : xs, x)

-- match end of input
eof :: Parser [a] ()
eof = Parser $ \case
    [] -> Just ([], ())
    _ -> Nothing

-- match next token of input if it satisfies the predicate
satisfy :: (a -> Bool) -> Parser [a] a
satisfy p = do
  a <- consume
  if p a then pure a else empty

-- match next token of input if it is equal to the given token
char :: Eq a => a -> Parser [a] a
char c = satisfy (== c)

comma :: Parser [Char] Char
comma = char ','

testCommaInvalidInput :: Bool
testCommaInvalidInput = runParser comma "1" == Nothing

testCommaValidInput :: Bool
testCommaValidInput = runParser comma "," == Just ("", ',')

-- match a string of tokens
string :: Eq a => [a] -> Parser [a] [a]
string = foldr (liftA2 (:) . char) (pure [])

testString :: Bool
testString = runParser (string "as") "asd" == Just ("d", "as")

-- parse a list of values separated by a given separator
-- taken from: https://hackage.haskell.org/package/attoparsec-0.14.4/docs/src/Data.Attoparsec.Combinator.html#local-6989586621679068158

sepBy :: Parser s a -> Parser s b -> Parser s [a]
sepBy p sep = scan
    where scan = liftA2 (:) p ((sep *> scan) <|> pure [])

testSepBy :: Bool
testSepBy = runParser (sepBy (string "asd") comma) "asd,asd"
                                  == Just ("",["asd","asd"])

-- parse a value enclosed by two ignored parsers
between :: Parser s a -> Parser s b -> Parser s c -> Parser s c
between open close p = open *> p <* close

betweenTest :: Bool
betweenTest = runParser (between
                         (char '[')
                         (char ']')
                         (string "as")) "[as]"
              == Just ("","as")

-- parse a value or use a default value if the parser fails
option :: a -> Parser s a -> Parser s a
option x p = p <|> pure x

testOption :: Bool
testOption = runParser (option '.' comma) "asd" == Just ("asd", '.')

-- parse and skip zero or more space characters
skipSpaces :: ParserS ()
skipSpaces = scan $> ()
  where scan = (char ' ' *> scan) <|> pure []

testSkipSpaces :: Bool
testSkipSpaces = runParser skipSpaces "   ," == Just (",",())

-- parse a word as a on-empty string of lowercase letters
word :: ParserS String
word = some $ satisfy (`elem` (['a' .. 'z'] :: String))

-- parse a natural number
natural :: ParserS Integer
natural = read <$> many (satisfy isDigit)

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
