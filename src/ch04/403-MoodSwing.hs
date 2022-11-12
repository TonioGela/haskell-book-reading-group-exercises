module Ch04.MoodSwing where  

data Mood = Blah | Woot deriving Show

-- What is the type constructor, or name of this type?
-- Mood

-- If the function requires a Mood value, what are the values you could possibly use?
-- Blah or Woot

-- We are trying to write a function changeMood to change Chris’s mood instantaneously.
--  It should act like not in that, given one value, it returns the other value of the same type.
--  So far, we’ve written a type signature changeMood :: Mood -> Woot. What’s wrong with that?
-- It always returns Woot

-- Now we want to write the function that changes his mood. Given an input mood, it gives us the other one.
--  Fix any mistakes and complete the function:
changeMood Blah = Woot
changeMood Woot = Blah
