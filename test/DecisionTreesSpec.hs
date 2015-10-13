-----------------------------------------------------------------------------
--
-- Module      :  DecisionTreesSpec
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

module DecisionTreesSpec (
  run
) where

--import Test.Hspec

import DecisionTrees
import DecisionTrees.C45
import TestData.TiloBalkeExample



--spec :: Spec
--spec = undefined

run = print x
    where x = buildDecisionTree testData



