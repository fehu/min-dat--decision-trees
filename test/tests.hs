module Main (
    main
) where

import Test.Hspec

import TestData.TiloBalkeExample
import DecisionTreesSpec
import qualified AgeSpec
import qualified C45Spec


main :: IO ()
main = hspec $ do AgeSpec.spec
                  C45Spec.spec


