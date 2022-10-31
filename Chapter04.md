## 4.3 Anatomy of a data declaration

### Exercises: Mood swing

`data Mood = Blah | Woot deriving Show`

1. `Mood`
1. `Blah` or `Woot`
1. `Woot` is a data constructor _not_ a type constructor, the signature should be `changeMood :: Mood -> Mood`

## 4.6 Go on and Bool me

### Exercises: Find the mistakes

1. ❌ `not True && true`, `true` must be capitalized. Fix `not True && True`
1. ❌ `not (x = 6)`, single `=` is assignment, not equality. Fix `not (x == 6)`
1. ✅ `(1 * 2) > 5`
1. ❌ `[Merry] > [Happy]`, double quote delimiters are missing. Fixes `"Merry" > "Happy"`, or `["Merry"] > ["Happy"]`
1. ❌ `[1, 2, 3] ++ "look at me!"`, cannot concatenate lists w/ different types. Fix `['1', '2', '3'] ++ "look at me!"`

## 4.9 Chapter exercises

1. `length :: [a] -> Integer`, from GHCi `length :: Foldable t => t a -> Int`
1. results:
   1. `length [1, 2, 3, 4, 5]` -> `5`
   1. `length [(1, 2), (2, 3), (3, 4)]` -> `3`
   1. `length allAwesome` -> `2`
   1. `length (concat allAwesome)` -> `5`
