module Chapter03.Scope where

-- area d = pi * (r * r)

-- r = d / 2 -- d not in scope

area d = pi * (r * r)
  where
    r = d / 2