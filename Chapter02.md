## 2.6 Infix operators

### Exercises: Parentheses and association

1. `8 + 7 * 9` differs from `(8 + 7) * 9`, the former evaluates to `71`, the latter to `135`
2. `perimeter x y = (x * 2) + (y * 2)` is equivalent to `perimeter x y = x * 2 + y * 2` because `*` has higher precedence than `+`
3. `f x = x / 2 + 9` differs from `f x = x / (2 + 9)` because `+` has a lower precedence than `/`

## 2.7 Declaring values

### Exercises: Heal the sick

1. error loading file in GHCi

```
src/Chapter02.hs:10:1: error:
    • Could not deduce (Num (b0 -> c))
        arising from a type ambiguity check for
        the inferred type for ‘area’
        (maybe you haven't applied a function to enough arguments?)
      from the context: (Num (b -> c), Num (a -> c), Num (a -> b))
        bound by the inferred type for ‘area’:
                   forall {b} {c} {a}.
                   (Num (b -> c), Num (a -> c), Num (a -> b)) =>
                   (a -> c) -> a -> c
        at src/Chapter02.hs:10:1-24
      The type variable ‘b0’ is ambiguous
    • In the ambiguity check for the inferred type for ‘area’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      When checking the inferred type
        area :: forall {b} {c} {a}.
                (Num (b -> c), Num (a -> c), Num (a -> b)) =>
                (a -> c) -> a -> c
   |
10 | area x = 3. 14 * (x * x)
   | ^^^^^^^^^^^^^^^^^^^^^^^^
```

2. error loading file in GHCi

```
src/Chapter02.hs:12:12: error: Variable not in scope: b
   |
12 | double x = b * 2
   |            ^
```

3. error loading file in GHCi

```
src/Chapter02.hs:15:4: error:
    parse error on input ‘=’
    Perhaps you need a 'let' in a 'do' block?
    e.g. 'let x = 5' instead of 'x = 5'
   |
15 |  y = 10
   |    ^
```

## 2.10 `let` and `where`

### Exercises: A head code

1. `let x = 5 in x` -> evaluates to `5`
1. `let x = 5 in x * x` -> evaluates to `25`
1. `let x = 5; y = 6 in x * y` -> evaluates to `30`
1. `let x = 3; y = 1000 in x + 3` -> evaluates to `6`
