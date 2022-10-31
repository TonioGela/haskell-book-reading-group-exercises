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
1. `6 / length [1, 2, 3]` returns an error because the return type of `length` is `Int` which doesn't implement `Fractional` type class
1. `` 6 `div` length [1, 2, 3] ``
1. `2 + 3 == 5`, type `Bool`, result `True`
1. types and results:
   1. `x = 5`
   1. `x + 3 == 5`, type `Bool`, result `False`
1. results:
   1. ✅ `length allAwesome == 2` -> `True`
   1. ❌ `length [1, 'a', 3, 'b']`, elements in the list have different types
   1. ✅ `length allAwesome + length awesome` -> `5`
   1. ✅ `(8 == 8) && ('b' < 'a')` -> `False`
   1. ❌ `(8 == 8) && 9`, `9` is _not_ a `Bool` and `&&` works with `Bool` parameters
1. see [source code](./src/Chapter04/exercises.hs)
