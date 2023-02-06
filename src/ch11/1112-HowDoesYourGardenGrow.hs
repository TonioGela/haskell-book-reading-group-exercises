module Ch11.HowDoesYourGardenGrow where

data FlowerType = Gardenia | Daisy | Rose | Lilac deriving Show
type Gardener = String
data Garden = Garden Gardener FlowerType deriving Show

-- z*(a+b+c+d)
-- a*z + b*z + c*z + d*z

data Garden' = 
  Gardenia Gardener |
  Daisy Gardener |
  Rose Gardener |
  Lilac Gardener
  deriving Show
