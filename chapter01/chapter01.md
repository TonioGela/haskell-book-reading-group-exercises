# Combinators

| expr           | combinator |
|----------------|------------|
`λx.xxx`         | yes       
`λxy.zx`         | no: free z
`λxyz.xy(zx)`    | yes       
`λxyz.xy(zxy)`   | yes       
`λxy.xy(zxy)`    | no: free z


# Normal form

| expr           | divergence     |
|----------------|----------------|
`λx.xxx`         | is normal form
`(λz.zz)(λy.yy)` | diverges
`(λx.xxx)z`      | normal form: zzz

# Beta reduce

## `(λabc.cba)zz(λwv.w)`
- `([a := z]λbc.cbz)z(λwv.w)`
- `([b := z]λc.czz)(λwv.w)`
- `[c := (λwv.w)](λwv.w)zz`
- `([w := z]λv.z) z`
- `[v := z] z`
- `z`

## `(λx.λy.xyy)(λa.a)b`
- `([x := (λa.a)]λy.(λa.a)yy)b`
- `[y := b](λa.a)bb`
- `[a := b]bb`
- `bb`

## `(λy.y)(λx.xx)(λz.zq)`
- `[y := (λx.xx)](λx.xx)(λz.zq)`
- `[x := (λz.zq)](λz.zq)(λz.zq)`
- `[z := (λz.zq)](λz.zq)q`
- `[z := q]qq`
- `qq`

## `(λz.z)(λz.zz)(λz.zy)`
- alpha equivalent `(λa.a)(λb.bb)(λc.cy)`
- `[a := (λb.bb)](λb.bb)(λc.cy)`
- `[b := (λc.cy)](λc.cy)(λc.cy)`
- `[c := (λc.cy)](λc.cy)y`
- `[c := y]yy`
- `yy`

## `(λx.λy.xyy)(λy.y)y`
- alpha equivalent `(λa.λb.abb)(λc.c)y`
- `[a := (λc.c)](λb.(λc.c)bb)y`
- `[b := y](λc.c)yy`
- `[c := y]yy`
- `yy`

## `(λa.aa)(λb.ba)c`
- `[a := (λb.ba)](λb.ba)(λb.ba)c`
- `[b := (λb.ba)](λb.ba)ac`
- `[b := a]aac`
- `aac`

## `(λxyz.xz(yz))(λx.z)(λx.a)`
- alpha equivalent `(λxyk.xk(yk))(λx.z)(λx.a)`
- `([x := (λx.z)]λyk.(λx.z)k(yk))(λx.a)`
- `([y := (λx.a)]λk.(λx.z)k((λx.a)k))`
- `λk.([x := k]z)((λx.a)k))`
- `λk.z((λx.a)k))`
- `λk.z([x := k]a))`
- `λk.za`

