module Ch13.ChapterExercises where

import System.Exit (exitSuccess)
import Control.Monad
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = keepOnlyLetters $ stringToLower $ line1
  case (line2 == reverse line2) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

orda = ord 'a'
ordz = ord 'z'
keepOnlyLetters :: String -> String
keepOnlyLetters = filter (\c -> if ord c >= orda && ord c <= ordz then True else False)  

stringToLower :: String -> String
stringToLower = fmap toLower

-- then "Madam I’m Adam," is palindrome

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please, prompt the name: "
  name <- getLine
  putStrLn "Please, prompt the age: "
  age <- getLine
  let p = mkPerson name (read age)
  case p of
    (Right p) -> do
      putStrLn $ "Yay! Successfully got a person: " ++ show p
      exitSuccess
    (Left p)  -> do
      putStrLn $ "Error: " ++ show p
      exitSuccess
