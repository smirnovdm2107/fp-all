module T2Test (testT2) where

import Data.Monoid

import Test.Hspec
import Test.Hspec.QuickCheck

import HW3.T1
import HW3.T2

testT2 :: SpecWith ()
testT2 = do
    describe "testing T2" $ do
        prop "Option homomorphism" $ do
            \x y -> distOption (wrapOption x, wrapOption y) `shouldBe` wrapOption (x :: Int, y :: Int)
        prop "Option associativity" $ do
            _ <- \x y z -> distOption (Some x, distOption (Some y, Some z)) `shouldBe` Some (x :: Int, (y :: Int, z :: Int))
            \x y z -> distOption (distOption (Some x, Some y), Some z) `shouldBe` Some ((x :: Int, y :: Int), z :: Int)
        prop "Pair homomorphism" $ do
            \x y -> distPair (wrapPair x, wrapPair y) `shouldBe` wrapPair (x :: Int, y :: Int)
        prop "Pair associativity" $ do
            _ <- \a1 a2 b1 b2 c1 c2 -> distPair (P a1 a2, distPair (P b1 b2, P c1 c2)) `shouldBe` P (a1 :: Int, (b1 :: Int, c1)) (a2 :: Int, (b2 :: Int, c2 :: Int))
            \a1 a2 b1 b2 c1 c2 -> distPair (distPair (P a1 a2, P b1 b2), P c1 c2) `shouldBe` P ((a1 :: Int, b1 :: Int), c1 :: Int) ((a2 :: Int, b2 :: Int), c2 :: Int)
        prop "Quad homomorphism" $ do
            \x y -> distQuad (wrapQuad x, wrapQuad y) `shouldBe` wrapQuad (x :: Int, y :: Int)
        prop "Annotated homomorphism" $ do
            \x y -> distAnnotated (wrapAnnotated x :: Annotated (Sum Int) Int, wrapAnnotated y :: Annotated (Sum Int) Int) `shouldBe` wrapAnnotated (x, y)
        prop "Annotated associativity" $ do
            \a1 e1 a2 e2 a3 e3 -> distAnnotated (a1 :# e1, distAnnotated (a2 :# e2, a3 :# e3)) `shouldBe` ((a1 :: Int, (a2 :: Int, a3 :: Int)) :# ((e1 :: Sum Int) <> (e2 :: Sum Int) <> (e3 :: Sum Int)))