import Test.Hspec

import Control.Monad.State
import Data.Set
import Lib

main :: IO ()
main = hspec $ do
  describe "Line" $ do
    it "is created correctly" $ do
      runState (makeLine (fromList [Point "a", Point "b"]) "line") (fromList [], []) `shouldBe` ((), (fromList [Point "a", Point "b"], [SpaceSubset Line (fromList [Point "a", Point "b"]) "line"]))
    it "updates other lines correctly" $ do
      runState (makeLine (fromList [Point "a", Point "b"]) "no c" >> makeLine (fromList [Point "a", Point "b", Point "c"]) "has c") (fromList [], []) `shouldBe` ((), (fromList [Point "a", Point "b", Point "c"], [SpaceSubset Line (fromList [Point "a", Point "b", Point "c"]) "has c", SpaceSubset Line (fromList [Point "a", Point "b", Point "c"]) "no c"])) --there has to be a better way of doing this
