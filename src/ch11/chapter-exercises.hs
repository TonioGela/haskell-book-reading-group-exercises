module Ch11.ChapterExercises where

import Data.Char

-- Weekday is a type with five data constructors
data Weekday =
         Monday
       | Tuesday
       | Wednesday
       | Thursday
       | Friday

f :: Weekday -> String
f Friday = "Miller Time"

-- Types defined with the data keyword must begin with a capital letter

-- The function g is recursive and may not terminate
g xs = xs !! (length xs - 1)


caesarCipher :: Int -> String -> String
caesarCipher' i = map (chr.(+97).(flip mod 26).(subtract 97).(+i).ord)

vigenereCipher :: String -> String -> String