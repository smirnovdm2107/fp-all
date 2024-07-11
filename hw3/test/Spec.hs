module Main (main) where

import Test.Hspec

import T1Test
import T2Test
import T3Test
import T4Test

main :: IO ()
main = hspec $ do
    testT1
    testT2
    testT3
    testT4