module T3Test (task3Test) where
import           Test.Hspec

import           HW1.T3

task3Test :: SpecWith ()
task3Test =
    describe "Test task 3" $ do
        it "Test from list" $ do
            let
                tree = tFromList [1, 2, 3, 4, 5, 10]
                in map ((\f -> f tree) . tmember) [1::Int, 2, 3, 4, 5, 6 , 8, 10]
                `shouldBe`
                [True, True, True, True, True, False, False, True]
        it "Test insert" $ do
            let
                tree = tFromList [1..10]
                in tmember (11::Int) (tinsert 11 tree) `shouldBe` True

