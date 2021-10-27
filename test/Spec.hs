import           Fourteen.Addition
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $
            (1 + 1) > 1 `shouldBe` True
