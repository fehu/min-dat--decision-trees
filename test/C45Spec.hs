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

import Data.Typeable
import qualified Data.Set as Set
import Control.Arrow

import DecisionTrees.C45
import DecisionTrees.Learning
import DecisionTrees.Learning.Debug
import TestData.TiloBalkeExample

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

--        let ex4' = [[0,1,2],[0,1,1]]

        let f' (x, y) = [x, y]
        let f = map f'

        describe "entropy" $ do
            specify ("E" ++ show ex1 ++ " = 0.694") $ example $ round' 3 (entropy ex1) `shouldBe` 0.694
            specify ("E" ++ show ex2 ++ " = 0.4")   $ example $ round' 1 (entropy ex2) `shouldBe` 0.4
            specify ("E" ++ show ex3 ++ " = 0.97")  $ example $            entropy ex3 `shouldBe` 0
--            specify ("E" ++ show ex4' ++ " = 0")    $ example $          entropy' ex4' `shouldBe` 0

        describe "gain" $ do
            specify ("Gain" ++ show ex1 ++ " = 0.247") $ example $ round' 3 (gain ex1) `shouldBe` 0.247
            specify ("Gain" ++ show ex2 ++ " = 0.57")  $ example $ round' 2 (gain ex2) `shouldBe` 0.57
            specify ("Gain" ++ show ex3 ++ " = 0.97")  $ example $ round' 2 (gain ex3) `shouldBe` 0.97

            specify ("Gain" ++ show (f ex1) ++ " = 0.247") $ example $ round' 3 (gain' (f ex1)) `shouldBe` 0.247
            specify ("Gain" ++ show (f ex2) ++ " = 0.57")  $ example $ round' 2 (gain' (f ex2)) `shouldBe` 0.57
            specify ("Gain" ++ show (f ex3) ++ " = 0.97")  $ example $ round' 2 (gain' (f ex3)) `shouldBe` 0.97

--            specify ("Gain" ++ show ex4' ++ " = 0")  $ example $ gain' ex4' `shouldBe` 0

--        context "given a precalculated example" $
--            describe "must behave as the example given" $ do

        let f (Attr x) = case (cast x :: Maybe Age) of Just (Age i) -> i <= 30
        let d1  = filter (f . attrByName (AttrName "age")) testData
        let d1e = Set.singleton $ AttrName "age"
        -- ([AttrValSet], Float)
        let d1r = ([ (AttrName "student", Set.singleton . Attr $ Student True)
                   , (AttrName "student", Set.singleton . Attr $ Student False)
                   ]
                  , 0.97)
        let p = second (round' 2)
        describe "selectBestAttrSplitting" $ do
            specify ("selectBestAttrSplitting ")  $ example $ p (selectBestAttrSplitting  d1 d1e) `shouldBe` d1r
--            specify ("selectBestAttrSplitting' ") $ example $ selectBestAttrSplitting' d1 d1e `shouldBe` [(d1r, [])]

