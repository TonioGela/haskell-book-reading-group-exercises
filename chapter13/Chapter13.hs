module Chapter13 () where

import Control.Monad (forever, when)
import Data.Char (toLower, isLetter)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

--------------------
---Chapter's code---
--------------------
type WordList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxNumberErrors :: Int
maxNumberErrors = 7

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do
  filter gameLength <$> allWords
  where gameLength w = let l = length (w :: String)
                       in l > minWordLength
                          && l < maxWordLength

randomWord ::  WordList -> IO String
randomWord wl = do
  l <- length <$> gameWords
  randomIndex <- randomRIO (0, l)
  return $ wl !! randomIndex

{-
randomWord :: WordList -> IO String
randomWord wl = (wl !! )
                <$> (gameWords
                     >>= (\n -> randomRIO (0, n)) . length)
-}
randomWord' :: IO String
randomWord' = gameWords >>= randomWord


data Puzzle = Puzzle String [Maybe Char] [Char]

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar = maybe '_' id

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle st = Puzzle st (map (const Nothing) st) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle st _ _)  c = c `elem` st

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ mxs _) c = Just c `elem` mxs

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
            if wordChar == guessed
              then Just wordChar
              else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> putStrLn "You already guessed that\
                              \ character, pick something else!"
                 >> return puzzle
    (True, _) -> putStrLn "This character was in the word,\
                              \ filling in the word accordingly"
                 >> return (fillInCharacter puzzle guess)
    (False, _) -> putStrLn "This character wasn't in\
                                \ the word, try again."
                  >> return (fillInCharacter puzzle guess)

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $ putStrLn "You win!" >> exitSuccess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when (length guessed > maxNumberErrors)
  $ putStrLn  "You lose!"
    >> putStrLn ("The word was: " ++ wordToGuess)
    >> exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
              gameOver puzzle
              gameWin puzzle
              putStrLn $ "Current puzzle is: " ++ show puzzle
              putStr "Guess a letter: "
              guess <- getLine
              case guess of
                [c] -> handleGuess puzzle c >>= runGame
                _ -> putStrLn "Your guess must\
                    \ be a single character"

main :: IO ()
main = gameWords >>= randomWord >>= (runGame . freshPuzzle . map toLower)

------------------------
---Hangman game logic---
------------------------

gameOver' :: Puzzle -> Int  -> IO ()
gameOver' (Puzzle wordToGuess _ _) counter =
  when (counter == 0)
  $ putStrLn  "You lose!"
    >> putStrLn ("The word was: " ++ wordToGuess)
    >> exitSuccess

data Result = Guessed | NotGuessed | AlreadyGuessed

makeGuess :: Puzzle -> Char -> (Puzzle, Result)
makeGuess puzzle guess =
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> (puzzle, AlreadyGuessed)
    (True, _) -> (fillInCharacter puzzle guess, Guessed)
    (False, _) -> (fillInCharacter puzzle guess, NotGuessed)

guessHandler :: (Puzzle, Result) -> Int -> IO (Puzzle, Int)
guessHandler (puzzle, Guessed) n = putStrLn "You guessed that"
                                   >> pure (puzzle, n)
guessHandler (puzzle, NotGuessed) n = putStrLn "Try again"
                                      >> pure (puzzle, n - 1)
guessHandler (puzzle, AlreadyGuessed) n = putStrLn "You already guessed that"
                                          >> pure (puzzle, n)

runGame' :: Puzzle -> Int  -> IO ()
runGame' puzzle counter = do
              gameOver' puzzle counter
              gameWin puzzle
              putStrLn $ "Current puzzle is: " ++ show puzzle
              putStr "Guess a letter: "
              guess <- getLine
              case guess of
                [c] -> guessHandler (makeGuess puzzle c) counter
                         >>= uncurry runGame'
                _ -> putStrLn "Your guess must\
                    \ be a single character"

main' :: IO ()
main' = gameWords >>= randomWord >>= (flip runGame' 0 . freshPuzzle . map toLower)

--------------------
---Modifying code---
--------------------

---Exercises 2 and 3---
palindrome :: IO ()
palindrome = forever $ do
  line <- filter isLetter . map toLower <$> getLine
  if line == reverse line
   then putStrLn "It's a palindrome!"
   else putStrLn "Nope!" >> exitSuccess

---Exercise 4---
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                       | AgeTooLow
                       | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | age <= 0 = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

handleResult :: Either PersonInvalid Person -> IO()
handleResult (Left NameEmpty) = putStrLn "Empty Name"
handleResult (Left AgeTooLow) = putStrLn "Age too low"
handleResult (Left (PersonInvalidUnknown st)) = putStrLn $ "Unknown error" ++ st
handleResult (Right person) = putStrLn $ "Yay! " ++ show person

gimmePerson :: IO ()
gimmePerson = do
        putStrLn "Enter a name:"
        name <- getLine
        putStrLn "Enter a number:"
        age <- read <$> getLine
        handleResult (mkPerson name age)
