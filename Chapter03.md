## 3.4 Top-level versus local definitions

### Exercises: Scope

1. Yes, `y` is in scope
1. No, `h` is _not_ in scope
1. No, `d` is _not_ in scope for `r`
1. Yes, everything is in scope

## 3.5 Types of concatenation functions

### Exercises: Syntax errors

1. `++ [1, 2, 3] [4, 5, 6]` does _not_ compile because `++` is used as prefix operator without `()` -> `(++) [1, 2, 3] [4, 5, 6]`
1. `'<3' ++ ' Haskell'` does _not_ compile because `'` is used instead of `"` for literal strings -> `"<3" ++ " Haskell"`
1. `concat ["<3", " Haskell"]` compiles because strings are lists of chars

## 3.8 Chapter exercises

### Reading syntax

#### 1.

<lettered>

1. `concat [[1, 2, 3], [4, 5, 6]]` ✅ -> `[1, 2, 3, 4, 5, 6]`
1. `++ [1, 2, 3] [4, 5, 6]` ❌, fix `(++) [1, 2, 3] [4, 5, 6]` -> `[1, 2, 3, 4, 5, 6]`
1. `(++) "hello" " world"` ✅ -> `"hello world"`
1. `["hello" ++ " world]` ❌, fix `["hello" ++ " world"]` -> `["hello world"]`
1. `4 !! "hello"` ❌, fix `"hello" !! 4` -> `'o'`
1. `(!!) "hello" 4` ✅ -> `'o'`
1. `take "4 lovely"` ❌, fix `take 4 "lovely"` -> `"love"`
1. `take 3 "awesome"` ✅ -> `"awe"`

</lettered>

#### 2.

<lettered>

1. `concat [[1 * 6], [2 * 6], [3 * 6]]` -> `[6, 12, 18]`
1. `"rain" ++ drop 2 "elbow"` -> `"rainbow"`
1. `10 * head [1, 2, 3]` -> `10`
1. `(take 3 "Julie") ++ (tail "yes")` -> `"Jules"`
1. `concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]]` -> `[2, 3, 5, 6, 8, 9]`

</lettered>

<style type="text/css">
    lettered ol { list-style-type: upper-alpha; }
</style>
