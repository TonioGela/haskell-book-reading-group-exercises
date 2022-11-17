import Test.Hspec
import qualified Nivlude
import Nivlude(NivludeException(..))
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "head" $ do
    it "returns the head of a non-empty list" $ do
      Nivlude.head [1,2,3] `shouldBe` 1
    it "returns the head of a list with a single element" $ do
      Nivlude.head [1] `shouldBe` 1
    it "throws exception on empty list" $ do
      evaluate (Nivlude.head []) `shouldThrow` (== NivludeException "empty list")

  describe "tail" $ do
    it "returns the tail of a non-empty list" $ do
      Nivlude.tail [1,2,3] `shouldBe` [2, 3]
    it "returns the empty list when applied to a list with a single element" $ do
      Nivlude.tail [1] `shouldBe` []
    it "throws exception on empty list" $ do
      evaluate (Nivlude.tail []) `shouldThrow` (== NivludeException "empty list")