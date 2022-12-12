module MyPrelude where

-- same as head
myHead :: [a] -> a
myHead (a:_) = a

-- same as last
myLast :: [a] -> a
myLast (a:[]) = a
myLast (_:as) = myLast as

-- same as tail
myTail :: [a] -> [a]
myTail (_:as) = as

-- same as (++)
(+++) :: [a] -> [a] -> [a]
(+++) [] b = b
(+++) (a:as) b = a : (as +++ b)

-- same as init
myInit :: [a] -> [a]
myInit (_:[]) = []
myInit (a:as) = [a] +++ myInit as

-- same as (!!)
(!!!) :: [a] -> Int -> a
(!!!) (a:_) 0 = a
(!!!) (_:as) b = as !!! (b - 1)

-- same as null
myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

-- same as length
myLength :: [a] -> Int
myLength [] = 0
myLength a = go a 0
  where
    go (_:[]) len = len + 1
    go (_:as') len = go as' (len + 1)

-- same as reverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]

-- same as map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (a:as) = [(f a)] ++ myMap f as

--same as filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (a:as)
  | toBeFiltered = [a] ++ (myFilter f as)
  | not toBeFiltered = myFilter f as
  where toBeFiltered = f a

-- same as take
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n (a:as) = [a] ++ myTake (n - 1) as

-- same as drop
myDrop :: Int -> [a] -> [a]
myDrop 0 a = a
myDrop n (a:as) = myDrop (n - 1) as

-- same as splitAt
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 (a:as) = ([], as)
mySplitAt 1 (a:as) = ([a], as)
mySplitAt n (a:as) = ([a] ++ fst (mySplitAt (n - 1) as), snd (mySplitAt (n - 1) as))

-- same as takeWhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (a:as)
  | fa = [a] ++ myTakeWhile f as
  | otherwise = []
  where fa = f a

-- same as dropWhile
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f [] = []
myDropWhile f (a:as)
  | fa = myDropWhile f as
  | otherwise = a:as
  where fa = f a

-- same as elem
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem v (a:as)
  | v == a = True
  | otherwise = myElem v as
