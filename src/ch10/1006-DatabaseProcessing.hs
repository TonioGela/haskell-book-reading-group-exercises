module Ch10.DatabaseProcessing where

import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [
  DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
  DbNumber 9001,
  DbString "Hello, world!",
  DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate utc):as) = utc:filterDbDate as
filterDbDate (_:as) = filterDbDate as

accumulateUTCTime :: DatabaseItem -> [UTCTime] -> [UTCTime]
accumulateUTCTime dbi acc =
  case dbi of
    (DbDate utc) -> utc:acc
    _            -> acc
filterDbDateWithFoldr :: [DatabaseItem] -> [UTCTime]
filterDbDateWithFoldr = foldr accumulateUTCTime []

-- 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber []                = []
filterDbNumber ((DbNumber i):as) = i:filterDbNumber as
filterDbNumber (_:as) = filterDbNumber as

accumulateInteger a b = case a of
  (DbNumber i) -> i:b
  _            -> b
filterDbNumberWithFoldr :: [DatabaseItem] -> [Integer]
filterDbNumberWithFoldr = foldr accumulateInteger []

-- 3
aDate = UTCTime (fromGregorian 1492 10 12) (secondsToDiffTime 0)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = (foldl (\x y -> if x > y then x else y) aDate) . filterDbDate

-- 4
sumDb :: [DatabaseItem] -> Integer
sumDb = (foldl (+) 0) . filterDbNumber

-- 5
avgDb :: [DatabaseItem] -> Double
avgDb a = fromIntegral ((sumDb a)) / (fromIntegral (length . filterDbNumber $ a))