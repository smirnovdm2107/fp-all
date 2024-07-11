module T4Test (testT4) where

import HW3.T1
import HW3.T4

import Test.Hspec


testT4 :: SpecWith ()
testT4 = do
    describe "Testing t4" $ do
        it "testing math" $ do
            runS (eval 0) [] `shouldBe` (0 :# [])
            runS (eval 1) [] `shouldBe` (1 :# [])
            runS (eval (1+1)) [] `shouldBe` (2 :# [Add 1 1])
            runS (eval (1 + 1 * 2)) [] `shouldBe` (3 :# [Add 1 2, Mul 1 2])
            runS (eval (1 - 1 + 1 - 1)) [] `shouldBe` (0 :# [Sub 1 1, Add 0 1, Sub 1 1])
            runS (eval (20 * 10 + 4 / 2 - 8 * 10)) [] `shouldBe` (122 :# [Sub 202 80, Mul 8 10, Add 200 2, Div 4 2, Mul 20 10])