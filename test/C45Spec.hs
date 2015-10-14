-----------------------------------------------------------------------------
--
-- Module      :  C45Spec
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module C45Spec ( spec ) where

import Test.Hspec
import Test.QuickCheck

import DecisionTrees.C45

-- http://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
round' n f = (fromInteger . round $ f * (10^n)) / (10.0^^n)

spec :: Spec
spec = describe "DecisionTrees.C45" $ do

        describe "information" $ do
            specify "I(3, 2) = 0.971" $ example $ round' 3 (information 3 2) `shouldBe` 0.971
            specify "I(2, 3) = 0.971" $ example $ round' 3 (information 2 3) `shouldBe` 0.971
            specify "I(9, 5) = 0.94"  $ example $ round' 2 (information 9 5) `shouldBe` 0.94
            specify "I(?) = 0"        $ property $ \x   -> information' [x]               `shouldBe` 0
            specify "I(?, 0, ...) = 0"$ property $ \x y -> information' (x:replicate y 0) `shouldBe` 0

        let ex1 = [(2,3), (4,0), (3,2)]
        let ex2 = [(1,0), (1,1), (0,2)]
        let ex3 = [(2,0), (0,3)]

        describe "entropy" $ do
            specify ("E" ++ show ex1 ++ " = 0.694") $ example $ round' 3 (entropy ex1) `shouldBe` 0.694
            specify ("E" ++ show ex2 ++ " = 0.4")   $ example $ round' 1 (entropy ex2) `shouldBe` 0.4
            specify ("E" ++ show ex3 ++ " = 0.97")  $ example $            entropy ex3 `shouldBe` 0

        describe "gain" $ do
            specify ("Gain" ++ show ex1 ++ " = 0.247") $ example $ round' 3 (gain ex1) `shouldBe` 0.247
            specify ("Gain" ++ show ex2 ++ " = 0.57")  $ example $ round' 2 (gain ex2) `shouldBe` 0.57
            specify ("Gain" ++ show ex3 ++ " = 0.97")  $ example $ round' 2 (gain ex3) `shouldBe` 0.97

