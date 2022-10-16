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

### Beta reduce

1.

```
(λabc.cba)zz(λwv.w)
(λa.λb.λc.cba)zz(λwv.w)
([a:=z].λb.λc.cba)z(λwv.w)
(λb.λc.cbz)z(λwv.w)
([b:=z].λc.cbz)(λwv.w)
(λc.czz)(λwv.w)
[c:=λwv.v].czz
(λwv.v)zz
(λw.λv.v)zz
([w:=z].λv.v)z
(λv.v)z
[v:=z].v
```

2.

```
(λx.λy.xyy)(λa.a)b
([x:=λa.a].λy.xyy)b
(λy.(λa.a)yy)b
[y:=b].(λa.a)yy
(λa.a)bb
([a:=b].a)b
bb
```

3.

```
(λy.y)(λx.xx)(λz.zq)
([y:=λx.xx].y)(λz.zq)
(λx.xx)(λz.zq)
[x:=λz.zq].xx
(λz.zq)(λz.zq)
[z:=λz.zq].zq
(λz.zq)q
[z:=q].zq
qq
```

4.

```
(λz.z)(λz.zz)(λz.zy)
(λz.z)(λa.aa)(λb.by)
([z:=λa.aa].z)(λb.by)
(λa.aa)(λb.by)
[a:=λb.by].aa
(λb.by)(λb.by)
[b:=λb.by].by
(λb.by)y
[b:=y].by
yy
```

5.

```
(λx.λy.xyy)(λy.y)y
(λx.λa.xaa)(λb.b)y
([x:=λb.b].λa.xaa)y
(λa.(λb.b)aa)y
[a:=y].(λb.b)aa
(λb.b)yy
([b:=y].b)y
yy
```

6.

```
(λa.aa)(λb.ba)c
(λx.xx)(λb.ba)c
([x:=λb.ba].xx)c
(λb.ba)(λb.ba)c
([b:=λb.ba].ba)c
(λb.ba)ac
([b:=a].ba)c
aac
```

7.

```
(λxyz.xz(yz))(λx.z)(λx.a)
(λtuv.tv(uv))(λx.z)(λx.a)
(λt.λu.λv.tv(uv))(λx.z)(λx.a)
([t:=λx.z].λu.λv.tv(uv))(λx.a)
(λu.λv.(λx.z)v(uv))(λx.a)
[u:=λx.a].λv.(λx.z)v(uv)
λv.(λx.z)v((λx.a)v)
λv.([x:=v].z)((λx.a)v)
λv.z((λx.a)v)
λv.z([x:=v].a)
λv.za
```
