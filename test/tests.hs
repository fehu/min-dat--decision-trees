module Main (
    main
) where

import Test.Hspec

import TestData.TiloBalkeExample
import qualified AgeSpec
import qualified ID3Spec
import qualified FullSubsetsSpec


main :: IO ()
main = hspec $ do AgeSpec.spec
                  ID3Spec.spec
                  FullSubsetsSpec.spec


