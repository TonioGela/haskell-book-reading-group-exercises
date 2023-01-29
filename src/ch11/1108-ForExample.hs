module Ch11.ForExample where

data Example = MakeExample deriving Show
-- :t MakeExample :: Example
-- :t Example -> error, use :k

data Example' = MakeExample' Int deriving Show
-- :t MakeExample' :: Int -> Example'
