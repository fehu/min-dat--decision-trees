module Main (
    main
) where

import Test.Hspec

import TestData.TiloBalkeExample
import qualified AgeSpec
import qualified C45Spec
import qualified FullSubsetsSpec


main :: IO ()
main = hspec $ do AgeSpec.spec
                  C45Spec.spec
                  FullSubsetsSpec.spec


