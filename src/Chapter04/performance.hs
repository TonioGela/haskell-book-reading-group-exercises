{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chapter04.Exercises (isPalindrome, isPalindromeOptimized)
import Control.Monad (replicateM)
import Text.StringRandom

main = do
  ts <- replicateM 1000 $ stringRandomIO ".{1024}"
  let ss = map show ts
  let palindromes = map (\s -> s ++ reverse s) ss
  let ssWithPalindrome = ss ++ palindromes
  let ips = map isPalindrome ssWithPalindrome
  let ipos = map isPalindromeOptimized ssWithPalindrome
  print $ "Done " ++ show ips ++ ", " ++ show ipos
