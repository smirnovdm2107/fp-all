module T2Test (task2Test) where
import HW1.T2
import Test.Hspec
import GHC.Natural (Natural)

nOp :: (N -> N -> a) -> Natural -> Natural -> a
nOp op a b = op (nFromNatural a) (nFromNatural b) 

plus :: Natural -> Natural -> N
plus = nOp nplus  

sub :: Natural -> Natural -> Maybe N
sub = nOp nsub

mult :: Natural -> Natural -> N
mult = nOp nmult

cmp :: Natural -> Natural -> Ordering
cmp = nOp ncmp

task2Test :: SpecWith ()
task2Test =
    describe "Test task 2" $ do
        it "Test from natural" $ do
            nFromNatural 5 `shouldBe` (S . S . S . S . S $ Z)
            nFromNatural 0 `shouldBe` Z
            nFromNatural 1 `shouldBe` S Z
        it "Test plus" $ do
            5 `plus` 10 `shouldBe` nFromNatural 15
            0 `plus` 0 `shouldBe` nFromNatural 0
            0 `plus` 1 `shouldBe` nFromNatural 1
            1 `plus` 0 `shouldBe` nFromNatural 1
        it "Test sub" $ do
            0 `sub` 0 `shouldBe` Just Z
            0 `sub` 1 `shouldBe` Nothing
            1 `sub` 1 `shouldBe` Just Z
            20 `sub` 10 `shouldBe` Just (nFromNatural 10)
            10 `sub` 20 `shouldBe` Nothing
        it "Test mult" $ do
            0 `mult` 0 `shouldBe` Z
            20 `mult` 0 `shouldBe` Z
            0 `mult` 20 `shouldBe` Z
            1 `mult` 10 `shouldBe` nFromNatural 10
            10 `mult` 1 `shouldBe` nFromNatural 10
            18 `mult` 31 `shouldBe` nFromNatural 558
        it "Test comp" $ do
            0 `cmp` 0 `shouldBe` compare (0::Int) 0
            0 `cmp` 1 `shouldBe` compare (0::Int) 1
            1 `cmp` 0 `shouldBe` compare (1::Int) 0
            10 `cmp` 11 `shouldBe` compare (10::Int) 11