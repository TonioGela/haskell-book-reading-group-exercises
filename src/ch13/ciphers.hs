module Main (main) where

import Data.Char
import System.Exit (exitSuccess)

main :: IO ()
main = do
  putStrLn "Please, provide a key: "
  key <- getLine
  putStrLn "Please, provide a sentence to be encrypted: "
  sentence <- getLine
  putStrLn $ "Key is: " ++ key
  putStrLn $ "Sentence is: " ++ sentence
  let se = caesarCipher (read key) sentence
  putStrLn $ "Sentence encrypted is: " ++ se
  exitSuccess


caesarCipher :: Int -> String -> String
caesarCipher i = map (chr.(+97).(flip mod 26).(subtract 97).(+i).ord)
