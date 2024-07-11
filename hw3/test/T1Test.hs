module T1Test (testT1) where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           HW3.T1

testT1 :: SpecWith ()
testT1 = do
    describe "Testing T1" $ do
        it "Testing mapOption" $ do
            mapOption (+1) (Some 1) `shouldBe` Some (2 :: Int)
            mapOption (*0) (Some 2) `shouldBe` Some (0 :: Int)
            mapOption id (Some 0) `shouldBe` Some (0 :: Int)
            mapOption id (Some "Haskell is soooo lovely!") `shouldBe` Some "Haskell is soooo lovely!"
            mapOption id None `shouldBe` (None :: Option Int) -- doesn't work with polymorf
            mapOption ("D+A="++) (Some "Love") `shouldBe` Some "D+A=Love"
        prop "mapOption id" $ do
            _ <- \x -> mapOption id (Some x) `shouldBe` Some (x :: Int)
            \_ -> mapOption id None `shouldBe` (None :: Option Int)
        prop "mapOption map" $ do
            \x -> (mapOption (10+) . mapOption (*2)) (Some x) `shouldBe` mapOption ((+10) . (*2)) (Some (x :: Int))
        it "Testing mapPair" $ do
            mapPair ("fu"++) (P "**" "rries") `shouldBe` P "fu**" "furries"
            mapPair unwords (P ["Hello", "world"] ["I", "love", "haskell"]) `shouldBe` P "Hello world" "I love haskell"
            mapPair id (P "a" "b") `shouldBe` P "a" "b"
        prop "mapPair id" $ do
            \x y -> mapPair id (P x y) `shouldBe` P x (y :: Int)
        prop "mapPair map" $ do
            \x y -> (mapPair (+10) . mapPair (*2)) (P x y) `shouldBe` mapPair ((+10) . (*2)) (P x (y :: Int))
        it "Testing mapQuad" $ do
            mapQuad (\x -> x++x) (Q "a" "b" "c" "d") `shouldBe` Q "aa" "bb" "cc" "dd"
            mapQuad (*10) (Q 1 2 3 4) `shouldBe` Q 10 20 30 (40 :: Int)
        prop "mapQuad id" $ do
            \a b c d -> mapQuad id (Q a b c d) `shouldBe` Q a b c (d :: Int) 
        prop "mapQuad map" $ do
            \a b c d -> (mapQuad (+10) . mapQuad (*2)) (Q a b c d) `shouldBe` mapQuad ((+10) . (*2)) (Q a b c (d :: Int))
        it "Testing mapAnnotated" $ do
            mapAnnotated ("No"++) ("money" :# "InBank") `shouldBe` ("Nomoney" :# "InBank")
        prop "mapAnnotated id" $ do
            \x y -> mapAnnotated id (x :# y) `shouldBe` ((x :: Int) :# (y :: Int))
        prop "mapAnnotated map" $ do
            \a e p m -> (mapAnnotated (+p) . mapAnnotated (*m)) (a :# e) `shouldBe` mapAnnotated ((+p) . (*m)) ((a :: Int) :# (e :: Int))
        it "Testing mapExcept" $ do
            mapExcept (++" is mine") (Success "your soul") `shouldBe` (Success "your soul is mine" :: (Except String String))
            mapExcept (++" is mine") (Error "nope") `shouldBe` (Error "nope" :: (Except String String))
        prop "mapExcept id" $ do
            _ <- \x -> mapExcept id (Success x) `shouldBe` (Success x :: (Except Int Int))
            \x -> mapExcept id (Error (x :: Int)) `shouldBe` (Error x :: (Except Int Int))
        prop "mapExcept map" $ do
            _ <- \x p m -> (mapExcept (+p) . mapExcept (*10)) (Success x) `shouldBe` mapExcept ((+p) . (*m)) (Success x :: (Except Int Int))
            \x p m -> (mapExcept (+p) . mapExcept (*10)) (Error x) `shouldBe` mapExcept ((+p). (*m)) (Error x :: (Except Int Int))
        it "Testing mapPrioritised" $ do
            mapPrioritised (++"Yes") (Low "No") `shouldBe` Low "NoYes"
            mapPrioritised (++"Yes") (Medium "No") `shouldBe` Medium "NoYes"
            mapPrioritised (++"Yes") (High "No") `shouldBe` High "NoYes"
        prop "mapPrioritised id" $ do
            _ <- \x -> mapPrioritised id (Low x) `shouldBe ` Low (x :: Int)
            _ <- \x -> mapPrioritised id (Medium x) `shouldBe` Medium (x :: Int)
            \x -> mapPrioritised id (High x) `shouldBe` High (x :: Int)
        prop "mapPrioritised map" $ do
            _ <- \x p m -> (mapPrioritised (+p) . mapPrioritised (*m)) (Low x) `shouldBe` mapPrioritised ((+p) . (*m)) (Low (x :: Int))
            _ <- \x p m -> (mapPrioritised (+p) . mapPrioritised (*m)) (Medium x) `shouldBe` mapPrioritised ((+p) . (*m)) (Medium (x :: Int)) 
            \x p m -> (mapPrioritised (+p) . mapPrioritised (*m)) (High x) `shouldBe` mapPrioritised ((+p) . (*m)) (High (x :: Int))
        it "Testing mapList" $ do
            mapList (++"abacaba") ("a_" :. "b_" :. "c_" :. Nil) `shouldBe` "a_abacaba" :. "b_abacaba" :. "c_abacaba" :. Nil
            mapList (*10) (0 :. 2 :. 10 :. Nil) `shouldBe` 0 :. 20 :. (100 :: Int) :. Nil 