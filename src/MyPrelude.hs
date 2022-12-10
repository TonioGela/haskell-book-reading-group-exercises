module MyPrelude where

-- same as head function
myHead :: [a] -> a
myHead (a:_) = a

-- same as last function
myLast :: [a] -> a
myLast (a:[]) = a
myLast (_:as) = myLast as

-- same as tail function
myTail :: [a] -> [a]
myTail (_:as) = as

-- same as (++) function
(+++) :: [a] -> [a] -> [a]
(+++) [] b = b
(+++) (a:as) b = a : (as +++ b)

-- same as init function
myInit :: [a] -> [a]
myInit (_:[]) = []
myInit (a:as) = [a] +++ myInit as

-- same as (!!) function
(!!!) :: [a] -> Int -> a
(!!!) (a:_) 0 = a
(!!!) (_:as) b = as !!! (b - 1)

-- same as null function
myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

-- same as length function
myLength :: [a] -> Int
myLength [] = 0
myLength a = go a 0
  where
    go (_:[]) len = len + 1
    go (_:as') len = go as' (len + 1)

-- same as reverse function
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]

-- same as map function
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (a:as) = [(f a)] ++ myMap f as

--same as filter function
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (a:as)
  | toBeFiltered = [a] ++ (myFilter f as)
  | not toBeFiltered = myFilter f as
  where toBeFiltered = f a

-- same as take function
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n (a:as) = [a] ++ myTake (n - 1) as

-- same as drop function
myDrop :: Int -> [a] -> [a]
myDrop 0 a = a
myDrop n (a:as) = myDrop (n - 1) as

-- same as splitAt function
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 (a:as) = ([], as)
mySplitAt 1 (a:as) = ([a], as)
mySplitAt n (a:as) = ([a] ++ fst (mySplitAt (n - 1) as), snd (mySplitAt (n - 1) as))
