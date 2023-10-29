{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Chapter24 () where

import Control.Applicative
import Control.Lens
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' <* eof

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

---Parser that parses either "1", "12", or "123" as a string value.

oneTwoThree :: Parser String
oneTwoThree = string "123" <|> string "12" <|> string "1"

---string parser implemented using char and applicative combinators.

string' :: String -> Parser String
string' = foldr (\c p -> (:) <$> char c <*> p) (pure [])

type NumberOrString' = Either Integer String

a :: String
a = "blah"

b :: String
b = "123"

c :: String
c = "123blah789"

parseNos :: Parser NumberOrString'
parseNos = (Left <$> integer) <|> (Right <$> some letter)

testNos :: IO ()
testNos = do
  print $ parseString parseNos mempty a
  print $ parseString parseNos mempty b
  print $ parseString parseNos mempty c
  print $ parseString (many parseNos) mempty c

parseNos' :: Parser NumberOrString'
parseNos' =
  skipMany (oneOf "\n")
    >> (Left <$> integer)
      <|> (Right <$> some letter)

-- Chapter Exercises

type Major = Integer

type Minor = Integer

type Patch = Integer

data NumberOrString = NOSS String | NOSI Integer
  deriving (Eq, Show)

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

majorP :: Parser Major
majorP = integer

minorP :: Parser Major
minorP = integer

patchP :: Parser Major
patchP = integer

nosP :: Parser NumberOrString
nosP = NOSS <$> some letter <|> NOSI <$> integer

dotP :: Parser Char
dotP = char '.'

nosListP :: Parser [NumberOrString]
nosListP = sepBy1 nosP dotP

releaseP :: Parser Release
releaseP = char '-' >> nosListP

metadataP :: Parser Metadata
metadataP = char '+' >> nosListP

semVerP :: Parser SemVer
semVerP = do
  major <- integer <* dot
  minor <- integer <* dot
  patch <- integer
  release <- option [] releaseP
  metadata <- option [] metadataP
  return $ SemVer major minor patch release metadata

testSemVerP :: IO ()
testSemVerP = do
  let result = parseString semVerP mempty "1.0.0-x.7.z.92"
  case preview _Success result of
    Just semver -> do
      putStrLn "Parsed successfully! The result is:"
      print semver
    Nothing -> putStrLn "Parsing failed."

parseDigit :: Parser Char
parseDigit = oneOf ['0' .. '9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit


base10Integer' :: Parser Integer
base10Integer' = (char '-') >> negate <$> base10Integer <|> base10Integer

{- Define the PhoneNumber type
data PhoneNumber = PhoneNumber Integer Integer Integer
    deriving (Eq, Show)

-- Parse an integer of a certain length, assuming it's all digits
parseFixedInt :: Int -> Parser Integer
parseFixedInt digitCount = read <$> count digitCount digit

-- Parse the area code, which could be in different formats
parseAreaCode :: Parser Integer
parseAreaCode =  (parens (parseFixedInt 3)) <|> parseFixedInt 3

-- Parser for the optional '1-' prefix
parseOnePrefix :: Parser (Maybe Char)
parseOnePrefix = optional $ try (char '1' >> char '-' >> return '1')

-- Main phone number parser function
parsePhone :: Parser PhoneNumber
parsePhone = do
    _ <- parseOnePrefix
    areaCode <- parseAreaCode
    optional (char '-')
    firstThree <- parseFixedInt 3
    optional (char '-' <|> char ' ')
    lastFour <- parseFixedInt 4
    return (PhoneNumber areaCode firstThree lastFour)
-}

type Prefix = Integer

type Subscriber = Integer

data PhoneNumb = PhoneNumb Prefix Subscriber
  deriving (Eq, Show)

prefixP :: Parser Prefix
prefixP = read <$> count 3 digit

internationalPrefixP :: Parser Prefix
internationalPrefixP = parseWithPlus <|> parseWithZeros
    parseWithPlus = char '+' >> count 2 digit >>= (return . read)
    parseWithZeros = string "00" >> count 2 digit >>= (return . read)

subscriberP :: Parser Subscriber
subscriberP = read <$> count 6 digit

phoneNumbP :: Parser PhoneNumb
phoneNumbP = do
  prefix <- prefixP <|> (parens prefixP)
  skipMany (oneOf " -.")
  subscriber <- subscriberP
  return $ (PhoneNumb prefix subscriber)

-- da aggiungere il prefisso internazionale e il fatto che il subscriber può essere separato da spazi o trattini
