import Test.Hspec

import qualified Ch1.Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
    describe "Ch1" Ch1.Spec.spec