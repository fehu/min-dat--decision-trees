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
import Data.Tree



--spec :: Spec
--spec = undefined

run = do x <- buildDecisionTree testData
         let tr = decision2Tree show x
         putStrLn ""
         putStrLn $ drawTree tr



