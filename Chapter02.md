## 2.6 Infix operators

### Excercises: Parentheses and association

1. `8 + 7 * 9` differs from `(8 + 7) * 9`, the former evaluates to `71`, the latter to `135`
2. `permiter x y = (x * 2) + (y * 2)` is equivalent to `permiter x y = x * 2 + y * 2` because `*` has higher precedence than `+`
3. `f x = x / 2 + 9` differs from `f x = x / (2 + 9)` because `+` has a lower precedence than `/`
