module Ch07.CasePractice where

-- We’re going to practice using case expressions by rewriting functions
functionC x y = if (x > y) then x else y
functionC' x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2' n =
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    EQ -> 0
    GT -> 1
