{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Chapter24 () where 

import Control.Applicative
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


type NumberOrString = Either Integer String

a :: String
a = "blah"
b :: String
b = "123"
c :: String
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

testNos :: IO ()
testNos = do
  print $ parseString parseNos mempty a
  print $ parseString parseNos mempty b
  print $ parseString parseNos mempty c
  print $ parseString (many parseNos) mempty c


parseNos' :: Parser NumberOrString
parseNos' = skipMany (oneOf "\n")
           >>
           (Left <$> integer)
           <|> (Right <$> some letter)
