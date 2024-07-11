module Main (main) where

import           T1Test
import           T2Test
import           T3Test
import           Test.Hspec


main :: IO ()
main = hspec $ do
   task1Test
   task2Test
   task3Test
