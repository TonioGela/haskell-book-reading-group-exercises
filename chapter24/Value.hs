{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies#-}
{-# LANGUAGE DeriveGeneric #-}


module Value (Value (..), Record (..))
where

import Data.Map (Map)
import Data.TreeDiff.Class (ToExpr)
import GHC.Generics (Generic)

-- a value is either an integer, a string, a list of values or a record
data Value = VInt Int | VString String | VList [Value] | VRecord Record
    deriving (Show, Eq, ToExpr, Generic)

-- a record is a map of values indexed by strings
newtype Record = Record (Map String Value)
    deriving (Show, Eq, Generic)
    deriving anyclass (ToExpr)
