-----------------------------------------------------------------------------
--
-- Module      :  C45Spec
-- License     :  MIT
--
-----------------------------------------------------------------------------

module C45Spec ( spec ) where

import Test.Hspec
import Test.QuickCheck

import Data.Typeable
import Data.Function
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow

import DecisionTrees
import DecisionTrees.C45
import DecisionTrees.Definitions hiding (Entry)
import TestData.TiloBalkeExample

-- http://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
round' n f = (fromInteger . round $ f * (10^n)) / (10.0^^n)

spec :: Spec
spec = do runIO . hspec $ specFuncs
          specExample


specFuncs :: Spec
specFuncs = describe "DecisionTrees.C45" $ do

        describe "information" $ do
            specify "I(3, 2) = 0.971" $ example $ round' 3 (information 3 2) `shouldBe` 0.971
            specify "I(2, 3) = 0.971" $ example $ round' 3 (information 2 3) `shouldBe` 0.971
            specify "I(9, 5) = 0.94"  $ example $ round' 2 (information 9 5) `shouldBe` 0.94
            specify "I(?) = 0"        $ property $ \x   -> information' [x]               `shouldBe` 0
            specify "I(?, 0, ...) = 0"$ property $ \x y -> information' (x:replicate y 0) `shouldBe` 0

        let ex1 = [(2,3), (4,0), (3,2)]
        let ex2 = [(1,0), (1,1), (0,2)]
        let ex3 = [(2,0), (0,3)]

        let f' (x, y) = [x, y]
        let f = map f'

        describe "entropy" $ do
            specify ("E" ++ show ex1 ++ " = 0.694") $ example $ round' 3 (entropy ex1) `shouldBe` 0.694
            specify ("E" ++ show ex2 ++ " = 0.4")   $ example $ round' 1 (entropy ex2) `shouldBe` 0.4
            specify ("E" ++ show ex3 ++ " = 0.97")  $ example $            entropy ex3 `shouldBe` 0

        describe "gain" $ do
            specify ("Gain" ++ show ex1 ++ " = 0.247") $ example $ round' 3 (gain ex1) `shouldBe` 0.247
            specify ("Gain" ++ show ex2 ++ " = 0.57")  $ example $ round' 2 (gain ex2) `shouldBe` 0.57
            specify ("Gain" ++ show ex3 ++ " = 0.97")  $ example $ round' 2 (gain ex3) `shouldBe` 0.97

--            specify ("Gain" ++ show (f ex1) ++ " = 0.247") $ example $ round' 3 (gain' (f ex1)) `shouldBe` 0.247
--            specify ("Gain" ++ show (f ex2) ++ " = 0.57")  $ example $ round' 2 (gain' (f ex2)) `shouldBe` 0.57
--            specify ("Gain" ++ show (f ex3) ++ " = 0.97")  $ example $ round' 2 (gain' (f ex3)) `shouldBe` 0.97


specExample :: Spec
specExample = let x = do res <- buildDecisionTree testData
                         let h = describe "DecisionTrees.C45" $
                                    context "given a precalculated example" $
                                        describe "must behave as the example given" $
                                            compareDecision 0 expectedDecision res
                         hspec h
              in runIO x

compareDecision :: Int -> Decision Entry AttributeContainer -> Decision Entry AttributeContainer -> Spec
compareDecision level ideal res =
    describe (ind' "  " ++ "on level " ++ show level) $ do
        let sameConstr = sameDecisionConstructor ideal res
        it (ind ++ "has same constructor") sameConstr
        if sameConstr
        then case (ideal, res) of (DecisionStep {select=si}, DecisionStep {select=sr})
                                        -> do it (ind ++ "DecisionSteps must have same forest") . example $
                                                shouldBe (Set.map show (Map.keysSet si))
                                                         (Set.map show (Map.keysSet sr))
                                              mapM_ (uncurry $ compareDecision (level+1))
                                                    (ordSelect si `zip` ordSelect sr)
                                  (Decision {classification=ci}, Decision{classification=cr})
                                        -> it (ind ++ "Decisions must have same classification") . example $
                                                ci `shouldBe` cr
        else it (ind ++ "must have same constructor") pending
    where ind = ind' "| "
          ind' p = replicate (4*level) ' ' ++ p

ordSelect :: (Ord a) => Map [a] b -> [b]
ordSelect = map snd . sortBy (compare `on` fst) . Map.toList

sameDecisionConstructor (DecisionStep {}) (DecisionStep {}) = True
sameDecisionConstructor (Decision {}) (Decision {})         = True
sameDecisionConstructor _ _                                 = False
