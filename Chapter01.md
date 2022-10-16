## 1.6 Multiple arguments

### Intermission: Equivalence exercises

1. `λxy.xz` -> replace `x` w/ `m` and `y` w/ `n` -> λmn.mz -> answer `b`
2. `λxy.xxy` -> replace `x` w/ `a`, replace `y` w/ `b` and use extend form form multiple arguments -> `λa.(λb.aab)` -> answer `c`
3. `λxyz.zx` -> replace `x` w/ `t`, `y` w/ `o` and `z` w/ `s` -> `λtos.st` -> answer `b`

## 1.11 Chapter exercises

### Combinators

A combinator is a lambda term with no free variables (cit.)

correct answers are `1`, `3` and `4`

### Normal form or diverge?

Divergence means that the reduction process never terminates ord ends (cit.)

1. `λx.xxx` -> cannot be reduced -> normal form
2. `(λz.zz)(λy.yy)` -> `(λy.yy)(λy.yy)` -> equivalent to `(λz.zz)(λy.yy)` -> diverge
3. `(λx.xxx)z` -> `zzz` -> cannot be reduced -> normal form
