module FullSubsetsSpec (spec) where

import Test.Hspec

import DecisionTrees.Utils
import qualified Data.Set as Set

spec :: Spec
spec = describe "DecisionTrees.Utils.fullSubsets" $
        it "should return a set of possible set splittings" $ do
            example $ fullSubsets (Set.fromList [1..2]) `shouldBe` Set.fromList
                [ Set.singleton $ Set.fromList [1..2]
                , Set.fromList $ map Set.singleton [1..2]
                ]
            example $ fullSubsets (Set.fromList [1..3]) `shouldBe` Set.fromList
                [ Set.singleton $ Set.fromList [1..3]
                , Set.fromList [ Set.singleton 1, Set.fromList [2, 3] ]
                , Set.fromList [ Set.singleton 2, Set.fromList [1, 3] ]
                , Set.fromList [ Set.singleton 3, Set.fromList [2, 1] ]
                , Set.fromList $ map Set.singleton [1..3]
                ]
            example $ fullSubsets (Set.fromList [1..4]) `shouldBe` Set.fromList
                [ Set.singleton $ Set.fromList [1..4]
                , Set.fromList [ Set.singleton 1, Set.fromList [2, 3, 4] ]
                , Set.fromList [ Set.singleton 1, Set.singleton 2, Set.fromList [3, 4] ]
                , Set.fromList [ Set.singleton 1, Set.singleton 3, Set.fromList [2, 4] ]
                , Set.fromList [ Set.singleton 1, Set.singleton 4, Set.fromList [2, 3] ]
                , Set.fromList [ Set.singleton 2, Set.fromList [1, 3, 4] ]
                , Set.fromList [ Set.singleton 2, Set.singleton 3, Set.fromList [1, 4] ]
                , Set.fromList [ Set.singleton 2, Set.singleton 4, Set.fromList [1, 3] ]
                , Set.fromList [ Set.singleton 3, Set.fromList [1, 2, 4] ]
                , Set.fromList [ Set.singleton 3, Set.singleton 4, Set.fromList [1, 2] ]
                , Set.fromList [ Set.singleton 4, Set.fromList [1, 2, 3] ]
                , Set.fromList $ map Set.singleton [1..4]
                ]

