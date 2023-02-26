module Chapter13 () where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

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

--- Personal note: fare un refactoring di sta funzione
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
                     putStrLn "You already guessed that\
                              \ character, pick something else!"
                     return puzzle
    (True, _) -> do
                     putStrLn "This character was in the word,\
                              \ filling in the word accordingly"
                     return (fillInCharacter puzzle guess)
    (False, _) -> do
                     putStrLn "This character wasn't in\
                                \ the word, try again."
                     return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if length guessed > 7
    then do putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do putStrLn "You win!"
            exitSuccess
    else return ()

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
main = do
         word <- randomWord'
         let puzzle = freshPuzzle (fmap toLower word)
         runGame puzzle
