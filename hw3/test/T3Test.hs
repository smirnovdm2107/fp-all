
module T3Test (testT3) where

import Data.Monoid

import HW3.T1
import HW3.T2
import HW3.T3


import Test.Hspec
import Test.Hspec.QuickCheck


testT3 :: SpecWith ()
testT3 = do
    describe "Testing t3" $ do
        prop "associativity option" $ do
            \x -> let wrapped = wrapOption . wrapOption . wrapOption $ (x :: Int)
                in joinOption (mapOption joinOption wrapped) `shouldBe` joinOption (joinOption wrapped) 
        prop "left and right identity option" $ do
            _ <- \x -> let wrapped = wrapOption . wrapOption $ (x :: Int)
                in joinOption (wrapOption wrapped) `shouldBe` wrapped
            \x -> let wrapped = wrapOption . wrapOption $ (x :: Int)
                in joinOption (mapOption wrapOption wrapped) `shouldBe` wrapped
        prop "associativity except" $ do
            \x -> let wrapped = wrapExcept . wrapExcept . wrapExcept  $ (x :: Int)
                in joinExcept (mapExcept joinExcept wrapped) `shouldBe` (joinExcept (joinExcept wrapped) :: Except (Sum Int) Int) 
        prop "left and right identity option" $ do
            _ <- \x -> let wrapped = wrapExcept . wrapExcept $ x
                in joinExcept (wrapExcept wrapped) `shouldBe` (wrapped :: Except (Sum Int) (Except (Sum Int) Int))
            \x -> let wrapped = wrapExcept . wrapExcept $ x
                in joinExcept (mapExcept wrapExcept wrapped) `shouldBe` (wrapped :: Except (Sum Int) (Except (Sum Int) Int))