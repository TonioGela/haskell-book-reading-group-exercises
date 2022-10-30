import Test.Hspec
import Chapter03

main :: IO ()
main = hspec $ do
  describe "Reading syntax 1" $ do
    it "concat [[1,2,3], [4,5,6]] is correct" $ do
      concat [[1,2,3], [4,5,6]] `shouldBe` [1,2,3,4,5,6]
    it "++ [1,2,3] [4,5,6] shouldBe (++) [1,2,3] [4,5,6]" $ do
      (++) [1,2,3] [4,5,6] `shouldBe` [1,2,3,4,5,6]
    it "(++) \"hello\" \" world\" is correct" $ do
      (++) "hello" " world" `shouldBe` "hello world"
    it "[\"hello\" ++ \" world\"] is correct => [\"hello world\"]" $ do
      ["hello" ++ " world"] `shouldBe` ["hello world"]
    it "4 !! \"hello\" shouldBe \"hello\" !! 4" $ do
      "hello" !! 4 `shouldBe` 'o'
    it "(!!) \"hello\" 4 is correct" $ do
      (!!) "hello" 4 `shouldBe` 'o'
    it "take \"4 lovely\" shouldBe take 4 \"lovely\"" $ do
      take 4 "lovely" `shouldBe` "love"
    it "take 3 \"awesome\" is correct" $ do
      take 3 "awesome" `shouldBe` "awe"

  describe "Reading syntax 2" $ do
    it "concat [[1 * 6], [2 * 6], [3 * 6]] === [6, 12, 18]" $ do
      concat [[1 * 6], [2 * 6], [3 * 6]] `shouldBe` [6, 12, 18]
    it "\"rain\" ++ drop 2 \"elbow\" === \"rainbow\"" $ do
      "rain" ++ drop 2 "elbow" `shouldBe` "rainbow"
    it "10 * head [1, 2, 3] === 10" $ do
      10 * head [1, 2, 3] `shouldBe` 10
    it "take 3 \"Julie\" ++ tail \"yes\" === \"Jules\"" $ do
      take 3 "Julie" ++ tail "yes" `shouldBe` "Jules"
    it "concat [[1,2,3], [4,5,6], [7,8,9]] === [1,2,3,4,5,6,7,8,9]" $ do
      concat [[1,2,3], [4,5,6], [7,8,9]] `shouldBe` [1,2,3,4,5,6,7,8,9]

  describe "Building functions" $ do
    it "funA \"Curry is awesome\" === \"Curry is awesome!\"" $ do
      fun1A "Curry is awesome" `shouldBe` "Curry is awesome!"
    it "funB \"Curry is awesome!\" === \"y\"" $ do
      fun1B "Curry is awesome!" `shouldBe` "y"
    it "funC \"Curry is awesome!\" === \"awesome!\"" $ do
      fun1C "Curry is awesome!" `shouldBe` "awesome!"
    it "thirdLetter \"Curry is awesome!\" === 'r'" $ do
      thirdLetter "Curry is awesome!" `shouldBe` 'r'
    it "letterIndex 1 == 'u'" $ do
      letterIndex 1 `shouldBe` 'u'
    it "rvrs \"Curry is awesome\" == \"awesome is Curry\"" $ do
      rvrs "Curry is awesome" `shouldBe` "awesome is Curry"
    

