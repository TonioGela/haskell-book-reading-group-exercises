## 4.3 Anatomy of a data declaration

### Exercises: Mood swing

`data Mood = Blah | Woot deriving Show`

1. `Mood`
1. `Blah` or `Woot`
1. `Woot` is a data constructor _not_ a type constructor, the signature should be `changeMood :: Mood -> Mood`
