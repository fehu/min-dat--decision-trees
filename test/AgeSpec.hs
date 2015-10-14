-----------------------------------------------------------------------------
--
-- Module      :  AgeSpec
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

module AgeSpec (
 spec
) where

import Test.Hspec
import TestData.TiloBalkeExample

spec :: Spec
spec = describe "Age" $ do
        describe "Age x == Age y" $ do
            specify "if  x == y " $ example $ Age 25 `shouldBe` Age (35-10)
            specify "iff x == y " $ example $ Age 25 `shouldNotBe` Age 26
        describe "Age x == AgeRange from to" $ do
            specify "if  x in [from, to]" $ do example $ Age 25 `shouldBe` AgeRange (Just 20) (Just 30)
                                               example $ Age 20 `shouldBe` AgeRange (Just 20) (Just 30)
                                               example $ Age 30 `shouldBe` AgeRange (Just 20) (Just 30)

                                               example $ Age 19 `shouldBe` AgeRange Nothing (Just 19)
                                               example $ Age 0  `shouldBe` AgeRange Nothing (Just 19)

                                               example $ Age 31 `shouldBe` AgeRange (Just 31) Nothing
                                               example $ Age 99 `shouldBe` AgeRange (Just 31) Nothing

            specify "iff x in [from, to]" $ do example $ Age 19 `shouldNotBe` AgeRange (Just 20) (Just 30)
                                               example $ Age 31 `shouldNotBe` AgeRange (Just 20) (Just 30)

