{-# LANGUAGE TypeSynonymInstances #-}

module Morra () where

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Trans.State (StateT, modify)
import System.Random (randomRIO)

data MorraNumber = Zero | One | Two | Three | Four | Five
  deriving (Eq, Ord, Enum, Show)

randomMorraNumber :: IO MorraNumber
randomMorraNumber = toEnum <$> randomRIO (0, 5)

mkMorraNumber :: Int -> Maybe MorraNumber
mkMorraNumber n
  | n >= 0 && n <= 5 = Just $ toEnum n
  | otherwise = Nothing

type Played = MorraNumber

type Guessed = MorraNumber

data Move = Move Guessed Played deriving (Eq, Show)

mkMove :: Int -> Int -> Maybe Move
mkMove guessed played = Move <$> mkMorraNumber guessed <*> mkMorraNumber played

askMove :: IO Move
askMove = do
  putStrLn "Enter your guessed number:"
  guessed <- read <$> getLine
  putStrLn "Enter your played number:"
  played <- read <$> getLine
  case mkMove guessed played of
    Just move -> return move
    Nothing -> do
      putStrLn "Invalid move, try again. Numbers must be between 0 and 5"
      askMove

mkRandomMove :: IO Move
mkRandomMove = Move <$> randomMorraNumber <*> randomMorraNumber

data Player = PlayerOne | PlayerTwo deriving (Eq, Show)

data Score = Score Int Int deriving (Eq, Show)

type Morra = StateT Score IO

updateScore :: Player -> Score -> Score
updateScore PlayerOne (Score p1 p2) = Score (p1 + 1) p2
updateScore PlayerTwo (Score p1 p2) = Score p1 (p2 + 1)

getWinner :: Move -> Move -> Maybe Player
getWinner (Move g1 p1) (Move g2 p2)
  | g1 == p2 && g2 == p1 = Nothing
  | g1 == p2 = Just PlayerOne
  | g2 == p1 = Just PlayerTwo
  | otherwise = Nothing

playRound :: IO (Maybe Player)
playRound = do
  putStrLn "Player One:"
  move1 <- askMove
  putStrLn "Player Two:"
  move2 <- mkRandomMove
  putStrLn $ "Player One: " ++ show move1
  putStrLn $ "Player Two: " ++ show move2
  return $ getWinner move1 move2

play :: Morra ()
play = do
  winner <- liftIO playRound
  case winner of
    Just player -> do
      modify $ updateScore player
      liftIO $ putStrLn $ "Player " ++ show player ++ " wins!"
    Nothing -> liftIO $ putStrLn "It's a tie!"
