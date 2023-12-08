{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Type
  (
  )
where

import Control.Monad.State ()
import Data.Char (isNumber)
import Data.Map (Map, fromList, lookup)

-- Shunting - Yard algorithm for Reverse Polish Notation
data Token
  = Number Int
  | ParenOpen
  | ParenClose
  | AddOp
  | MulOp
  | DivOp
  | SubOp
  deriving (Show, Eq)

isOp :: Token -> Bool
isOp AddOp = True
isOp MulOp = True
isOp DivOp = True
isOp SubOp = True
isOp _ = False

isWhiteSpace :: Char -> Bool
isWhiteSpace '\n' = True
isWhiteSpace '\r' = True
isWhiteSpace '\0' = True
isWhiteSpace _ = False

scanNumber :: String -> (Int, String)
scanNumber xs = (num, str)
  where
    (n, str) = span isNumber xs
    num = read n :: Int

opmap :: Map Char Token
opmap =
  fromList
    [ ('+', AddOp),
      ('*', MulOp),
      ('/', DivOp),
      ('(', ParenOpen),
      ('-', SubOp),
      (')', ParenClose)
    ]

tokenize :: String -> [Maybe Token]
tokenize s = loop s []
  where
    loop str tokens
      | Prelude.null str = tokens
      | isNumber $ head str =
          let (num, str') = scanNumber str
              tokens' = tokens ++ [Just (Number num)]
           in loop str' tokens'
      | isWhiteSpace $ head str = loop (tail str) tokens
      | otherwise = loop (tail str) (tokens ++ [Data.Map.lookup (head str) opmap])

prec :: Token -> Int
prec AddOp = 0
prec SubOp = 0
prec MulOp = 1
prec DivOp = 1
prec ParenOpen = 2
prec ParenClose = 2
prec (Number _) = 3

transform :: [Maybe Token] -> [Token]
transform ts = transform' ts [] []
  where
    -- No more tokens
    transform' [] [] q = q
    transform' [] s q =
      if head s == ParenOpen
        then error "Mismatched Parentheses"
        else transform' [] (tail s) (q ++ [head s])
    transform' (x : xs) s q = case x of
      Nothing -> error "Illegal tokens"
      (Just (Number n)) -> transform' xs s (q ++ [Number n])
      (Just ParenOpen) -> transform' xs (ParenOpen : s) q
      (Just ParenClose) -> transform' xs s0 q0
        where
          s0 = tail $ dropWhile (/= ParenOpen) s
          q0 = q ++ takeWhile (/= ParenOpen) s
      (Just o1) -> transform' xs s1 q1
        where
          cond o2 = isOp o2 && (prec o1 < prec o2)
          spl = span cond s
          s1 = o1 : snd spl
          q1 = q ++ fst spl

toString :: [Token] -> String
toString = concatMap toStringOne
  where
    toStringOne (Number n) = show n
    toStringOne AddOp = "+"
    toStringOne MulOp = "*"
    toStringOne DivOp = "/"
    toStringOne SubOp = "-"
    toStringOne ParenOpen = "("
    toStringOne ParenClose = ")"

convert :: String -> String
convert = toString . transform . tokenize

data Stack a where
  Empty :: Stack a
  Push :: a -> Stack a -> Stack a

instance Show a => Show (Stack a) where
  show Empty = ""
  show (Push x xs) = show x ++ " " ++ show xs

pop :: Stack a -> (a, Stack a)
pop Empty = error "Empty Stack"
pop (Push x xs) = (x, xs)

push :: a -> Stack a -> Stack a
push = Push

stack :: Stack Int
stack = Push 1 (Push 2 (Push 3 Empty))

eval :: [Token] -> Int
eval = eval' Empty

eval' :: Stack Int -> [Token] -> Int
eval' s [] = fst $ pop s
eval' s (Number n : xs) = eval' (push n s) xs
eval' s (AddOp : xs) = eval' s' xs
  where
    (n1, s') = pop s
    (n2, s'') = pop s'
    s''' = push (n1 + n2) s''
eval' s (MulOp : xs) = eval' s' xs
  where
    (n1, s') = pop s
    (n2, s'') = pop s'
    s''' = push (n1 * n2) s''
eval' s (SubOp : xs) = eval' s' xs
  where
    (n1, s') = pop s
    (n2, s'') = pop s'
    s''' = push (n2 - n1) s''
