module T1Test (task1Test) where

import           HW1.T1
import           Test.Hspec


task1Test :: SpecWith ()
task1Test =
     describe "Test task 1" $ do
        it "Test nextDay should be like in standard week" $ do
            nextDay Monday `shouldBe` Tuesday
            nextDay Tuesday `shouldBe` Wednesday
            nextDay Wednesday `shouldBe` Thursday
            nextDay Thursday `shouldBe` Friday
            nextDay Friday `shouldBe` Saturday
            nextDay Saturday `shouldBe` Sunday
            nextDay Sunday `shouldBe` Monday
        it "Test after days check" $ do
            afterDays 0 Monday `shouldBe` Monday
            afterDays 7 Sunday `shouldBe` Sunday
            afterDays 1 Tuesday `shouldBe` nextDay Tuesday
            afterDays 2 Wednesday `shouldBe` (nextDay . nextDay $ Wednesday)
            afterDays 30 Friday `shouldBe` Sunday
        it "Test is weekend" $ do
            isWeekend Monday `shouldBe` False
            isWeekend Sunday `shouldBe` True
            isWeekend Saturday `shouldBe` True
        it "Test days to party" $ do
            daysToParty Monday `shouldBe` 4
            daysToParty Friday `shouldBe` 0
            daysToParty Saturday `shouldBe` 6
