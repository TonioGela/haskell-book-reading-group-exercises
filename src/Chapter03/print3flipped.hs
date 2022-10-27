module Chapter03.Print3Flipped where

myGreeting = (++) "hello" " world!"

hello = "hello"

world = "world!"

main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where
    secondGreeting = (++) hello ((++) " " world)