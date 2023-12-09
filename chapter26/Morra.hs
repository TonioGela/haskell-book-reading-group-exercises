{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Morra
  ( askMove,
    getMove,
    getWinner,
    mkMove,
    mkPlayed,
    mkPlayers,
    playGame,
    playRound,
    updateScores,
    Winner (..),
    Move (..),
    Players,
    Scores,
    PlayerType (..),
    GameState (..),
  )
where

import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Trans.State (StateT, execStateT, get, modify)
import System.Random (randomRIO)

type Played = Int

mkPlayed :: Int -> Maybe Played
mkPlayed n
  | n >= 0 && n <= 5 = Just n
  | otherwise = Nothing

type Guessed = Int

data Move = Move Guessed Played deriving (Eq, Show)

mkMove :: Int -> Int -> Maybe Move
mkMove guessed played = Move guessed <$> mkPlayed played

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
mkRandomMove = Move <$> randomGuessed <*> randomPlayed
  where
    randomGuessed = randomRIO (0, 10)
    randomPlayed = randomRIO (0, 5)

data PlayerType = Human | AI deriving (Eq, Show, Read)

getMove :: PlayerType -> IO Move
getMove Human = askMove
getMove AI = mkRandomMove

type Players = (PlayerType, PlayerType)

mkPlayers :: IO Players
mkPlayers = do
  putStrLn "Enter first player type (Human or AI):"
  player1 <- read <$> getLine
  putStrLn "Enter second player type (Human or AI):"
  player2 <- read <$> getLine
  return (player1, player2)

type Scores = (Int, Int)

data Winner = First | Second | Tie
  deriving (Eq, Show)

data GameState = GameState
  { players :: Players,
    scores :: Scores
  }
  deriving (Eq, Show)

initialState :: IO GameState
initialState = do
  players <- mkPlayers
  return $ GameState players (0, 0)

getWinner :: Move -> Move -> Winner
getWinner (Move guessed1 played1) (Move guessed2 played2)
  | guessed1 == played1 + played2
      && guessed2 == played1 + played2 =
      Tie
  | guessed1 == played1 + played2 = First
  | guessed2 == played1 + played2 = Second
  | otherwise = Tie

updateScores :: Winner -> Scores -> Scores
updateScores First (x, y) = (x + 1, y)
updateScores Second (x, y) = (x, y + 1)
updateScores Tie s = s

type Morra = StateT GameState IO

playRound :: (PlayerType -> IO Move) -> Morra ()
playRound impureGet = do
  GameState players scores <- get
  let (player1, player2) = players
  move1 <- liftIO $ impureGet player1
  move2 <- liftIO $ impureGet player2
  let winner = getWinner move1 move2
  modify $ \s -> s {scores = updateScores winner scores}

winningScore :: Scores -> Bool
winningScore (x, y) = x == 5 || y == 5

winningMessage :: Scores -> String
winningMessage (x, y) =
  "Game over! Final scores: "
    ++ "Player 1 scored: "
    ++ show x
    ++ "Player 2 scored: "
    ++ show y

inplayMessage :: Scores -> String
inplayMessage (x, y) =
  "Current score: "
    ++ "Player 1: "
    ++ show x
    ++ "Player 2: "
    ++ show y

playGame :: Morra ()
playGame = do
  GameState _ scores <- get
  if winningScore scores
    then do
      liftIO $
        putStrLn $
          winningMessage scores
      return ()
    else do
      liftIO $
        putStrLn $
          inplayMessage scores
      playRound getMove >> playGame

main :: IO ()
main = do
  putStrLn "Welcome to Morra!"
  initialState >>= execStateT playGame
  putStrLn "Thanks for playing!"
