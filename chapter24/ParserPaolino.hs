{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module ParserPaolino () where

import Control.Applicative
  ( Alternative (..),
    Applicative (..),
    many,
    optional,
    some,
  )
import Control.Monad (MonadPlus (..))
import Data.Bool (Bool)
import Data.Char (isDigit, isSpace, ord)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.TreeDiff.Class (ToExpr)
import GHC.Base (undefined)
import GHC.Generics (Generic)
import Database.Redis (Reply(Integer))
import Text.Trifecta (HasErr(err))

-- | A parser is a function that may reduce the input and return a value in place
-- of the consumed input.
newtype Parser s a = Parser {runParser :: s -> Maybe (s, a)}

-- | Specific instance over [Char].
type ParserS = Parser String

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $ \s -> Just (s, x)
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
  empty = Parser $ const Nothing
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
  where
    scan = liftA2 (:) p ((sep *> scan) <|> pure []) <|> pure []

testSepBy :: Bool
testSepBy =
  runParser (sepBy (string "asd") comma) "asd,asd"
    == Just ("", ["asd", "asd"])

testSepByNothing :: Bool
testSepByNothing =
  runParser (sepBy (string "asd") comma) ""
    == Just ("", [])

-- parse a value enclosed by two ignored parsers
between :: Parser s a -> Parser s b -> Parser s c -> Parser s c
between open close p = open *> p <* close

betweenTest :: Bool
betweenTest =
  runParser
    ( between
        (char '[')
        (char ']')
        (string "as")
    )
    "[as]"
    == Just ("", "as")

-- parse a value or use a default value if the parser fails
option :: a -> Parser s a -> Parser s a
option x p = p <|> pure x

testOption :: Bool
testOption = runParser (option '.' comma) "asd" == Just ("asd", '.')

-- parse and skip zero or more space characters
skipSpaces :: ParserS ()
skipSpaces = scan $> ()
  where
    scan = (char ' ' *> scan) <|> pure []

testSkipSpaces :: Bool
testSkipSpaces = runParser skipSpaces "   ," == Just (",", ())

-- parse a word as a non-empty string of lowercase letters
word :: ParserS String
word = some $ satisfy (`elem` (['a' .. 'z'] :: String))

-- parse a natural number
natural :: ParserS Integer
natural = read <$> some (satisfy isDigit)

testNatural :: Bool
testNatural = runParser natural "001" == Just ("", 1)

-- parse an haskell-style list of values, use between and sepBy
list :: ParserS a -> ParserS [a]
list p = between open closed middle
  where
    open = char '[' *> skipSpaces
    middle = sepBy p (between skipSpaces skipSpaces comma)
    closed = skipSpaces <* char ']'

testList :: Bool
testList = runParser (list word) "[ a, b,c,d ]" == Just ("", ["a", "b", "c", "d"])

-- Additional exercices

-- a value is either an integer, a string, a list of values or a record
data Value = VInt Integer | VString String | VList [Value] | VRecord Record
  deriving (Show, Eq, ToExpr, Generic)

-- a record is a map of values indexed by strings
newtype Record = Record (Map String Value)
  deriving (Show, Eq, Generic)
  deriving anyclass (ToExpr)

createRecord :: [(String, Value)] -> Record
createRecord entries = Record (Map.fromList entries)

-- parse a value, use '<|>' to try different parsers
valueP :: ParserS Value
valueP =
  VInt <$> natural
    <|> VString <$> word
    <|> VList <$> list valueP
    <|> VRecord <$> recordP

-- parse a record
recordP :: ParserS Record
recordP = createRecord <$> between open closed middle
  where
    open = char '{' *> skipSpaces
    closed = char '}' *> skipSpaces
    middle = sepBy keyValueP (between skipSpaces skipSpaces comma)

-- parse a key-value pair key: value
keyValueP :: ParserS (String, Value)
keyValueP =
  word <* skipSpaces >>= \key ->
    char ':'
      >> skipSpaces
      >> valueP <* skipSpaces
      >>= \value ->
        pure (key, value)

testRecordP :: Bool
testRecordP =
  runParser recordP "{first: 3, second: 4, third: ciao}"
    == Just
      ( "",
        createRecord
          [ ("first", VInt 3),
            ("second", VInt 4),
            ("third", VString "ciao")
          ]
      )

testKeyValueP :: Bool
testKeyValueP = runParser keyValueP "key: 2" == Just ("", ("key", VInt 2))

data Result = Passed | Failed
  deriving (Show)

type Tests = [(String, Result)]

toRes :: Bool -> Result
toRes True = Passed
toRes False = Failed

tests :: Tests
tests =
  map
    (fmap toRes)
    [ ("testRecordP", testRecordP),
      ("testKeyValueP", testKeyValueP),
      ("testList", testList),
      ("testOption", testOption),
      ("testCommaInvalidInput", testCommaInvalidInput),
      ("testCommaValidInput", testCommaValidInput),
      ("testSepBy", testSepBy),
      ("testSkipSpaces", testSkipSpaces),
      ("testString", testString)
    ]

runTests :: Tests -> IO ()
runTests = mapM_ $ \(name, value) ->
  print (name ++ ": " ++ show value)

--------------------------------------------------------------------------------
--- Part 2: Arithmetic expressions' parser -------------------------------------
--------------------------------------------------------------------------------

item :: (Eq a) => a -> Parser [a] a
item c = satisfy (== c)

data Operator = Plus | Minus | Times | Div
  deriving (Show, Eq)

operation :: Operator -> Int -> Int -> Int
operation Plus = (+)
operation Minus = (-)
operation Times = (*)
operation Div = div

data Token
  = OpenParens
  | ClosedParens
  | Number Int
  | Operator Operator
  | Space -- discuss why Space is good to have
  deriving (Show, Eq)

-- tokenize a string
-- hint: use foldr!
-- think hard about how to tokenize numbers with more that one digit
-- allowed symbols in the expression are "0123456789 ()+-*/"
-- only (multiple digits) integers are allowed

tokenize :: String -> [Token]
tokenize = foldr f []
  where
    f :: Char -> [Token] -> [Token]
    f c tokens
      | c == ' ' = Space : tokens
      | c == '(' = OpenParens : tokens
      | c == ')' = ClosedParens : tokens
      | c == '+' = Operator Plus : tokens
      | c == '-' = Operator Minus : tokens
      | c == '*' = Operator Times : tokens
      | c == '/' = Operator Div : tokens
      | isDigit c = case tokens of
          (Number n : ts) -> Number (read [c] * 10 ^ length (show n) + n) : ts
          _ -> Number (read [c]) : tokens
      | otherwise = error "not an expression"

cleanSpaces :: [Token] -> [Token]
cleanSpaces = filter (/= Space)

testTokenize :: Bool
testTokenize =
  tokenize "(12 + 35) * 20"
    == [ OpenParens,
         Number 12,
         Space,
         Operator Plus,
         Space,
         Number 35,
         ClosedParens,
         Space,
         Operator Times,
         Space,
         Number 20
       ]

-- arithmetic expressions, we support operations on multiple operands
data Expression = Value Int | Operation Expression [(Operator, Expression)]
  deriving (Show, Eq)

data Precedence = Low | High
  deriving (Show, Eq, Ord)

precedence :: Operator -> Precedence
precedence Plus = Low
precedence Minus = Low
precedence Times = High
precedence Div = High

-- evaluate an expression respecting operator precedence inside multiple operands
-- operations
-- our operations are all left associatives so we can evaluate them in a single
-- pass from left to right
-- hint: use pattern matching to inspect the "next" operation

evaluate :: Expression -> Int
evaluate (Value n) = n
evaluate (Operation e []) = evaluate e
evaluate (Operation e1 [(op, e2)]) = operation op (evaluate e1) (evaluate e2)
evaluate (Operation e1 ((op1, e2) : (op2, e3) : exprs)) =
  if precedence op1 >= precedence op2
    then
      operation
        op2
        (operation op1 (evaluate e1) (evaluate e2))
        (evaluate (Operation (Value . evaluate $ e3) exprs))
    else
      operation
        op1
        (evaluate e1)
        (evaluate (Operation (Value . evaluate $ e2) ((op2, e3) : exprs)))

testEvaluate :: Bool
testEvaluate =
  evaluate (Operation (Value 2) [(Plus, Value 3), (Times, Value 4)])
    == 14

testEvaluate1 :: Bool
testEvaluate1 =
  evaluate (Operation (Value 2) [(Times, Value 3), (Plus, Value 4)])
    == 10



-- | A value is a token that is a number
valueEP :: Parser [Token] Expression
valueEP =  do
    (Number n)  <- consume
    pure . Value $ n

operatorP :: Parser [Token] Operator
operatorP = do
  (Operator op) <- consume
  pure op


--- Tentativo iniziale fallito
{-
parExprP :: Parser [Token] Expression
parExprP = between (item OpenParens)
                   (item ClosedParens)
                   expressionP <|> expressionP


-- | An operation is a
operationP :: Parser [Token] Expression
operationP = do
  expr <- parExprP
  ops <- many (liftA2 (,) operatorP parExprP)
  return $ Operation expr ops

-- a parser from a list of tokens to an expression
expressionP :: Parser [Token] Expression
expressionP = valueEP <|> operationP
-}

parExprP :: Parser [Token] Expression
parExprP = between (item OpenParens)
                   (item ClosedParens)
                   expressionP <|> valueEP -- hic est casus basis recursionis

expressionP :: Parser [Token] Expression
expressionP = expressionP' <|> valueEP
  where
    expressionP' = do
        expr <- parExprP
        ops <- many (liftA2 (,) operatorP parExprP)
        pure $ Operation expr ops

--- Runner
-- try to parse an arithmetic expression from a whole string
parse :: String -> Maybe ([Token], Int)
parse = parse' $ expressionP <* eof


parse' :: Parser [Token] Expression -> String -> Maybe ([Token], Int)
parse' f = fmap (fmap evaluate) . runParser f . cleanSpaces . tokenize

t :: String -> Int -> IO ()
t x r =
    if parse x == Just ([], r)
        then pure ()
        else
            error
                $ "assertion failed: "
                    ++ x
                    ++ " should be "
                    ++ show r
                    ++ " but we got "
                    ++ show
                        (parse x)


testss :: IO ()
testss = do
    t "1" 1
    t "(1)" 1
    t "10 + 2" 12
    t "1+2 + 3" 6
    t "1 + 2* 3" 7
    t "1 * 2 + 3" 5
    t "1 * 2 * 3" 6
    t "1 + 2 * 3 + 4" 11
    t "1 + 2 * 3 + 4 * 5" 27
    t "(1 + 2) * 3 + 4 * 5" 29
    t "((1 + 5))" 6
    t "((1 + 5) * 3)" 18
    t "((1+5)*3)+(4)" 22
    putStrLn "all tests passed"
