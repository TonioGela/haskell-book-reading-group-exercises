module Chapter03.Print3Broken where

main = do
  putStrLn greeting
  printSecond
  where
    greeting = "Yarrrrr"
    printSecond = putStrLn greeting
