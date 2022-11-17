module Nivlude (
    NivludeException(..),
    Nivlude.fst,
    Nivlude.snd,
    Nivlude.head,
    Nivlude.tail,
    Nivlude.reverse
) where

import Control.Exception

data NivludeException = NivludeException(String) deriving (Show, Eq)

instance Exception NivludeException

fst :: (a, b) -> a
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b

head :: [a] -> a
head [] = throw $ NivludeException "empty list"
head (a : _) = a

tail :: [a] -> [a]
tail [] = throw $ NivludeException "empty list"
tail (_ : as) = as

reverse :: [a] -> [a]
reverse xs = go xs []
    where
        go (x:xss) acc = go xss (x:acc)
        go [] acc = acc