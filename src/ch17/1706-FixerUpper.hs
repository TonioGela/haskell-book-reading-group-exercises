module Ch17.Lookups where


a = const <$> Just "Hello" <*> pure "World"

b = (,,,) <$> (Just 90) <*> (Just 10) <*> Just "Tierness" <*> pure [1, 2, 3]
