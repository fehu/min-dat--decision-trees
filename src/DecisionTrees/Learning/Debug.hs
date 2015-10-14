-----------------------------------------------------------------------------
--
-- Module      :  DecisionTrees.Learning.Debug
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

module DecisionTrees.Learning.Debug (
  TreeBranchingDebug(..)
) where

import DecisionTrees.Learning
import Data.Set (Set)

class (TreeBranching entry) =>
    TreeBranchingDebug entry where
        selectBestAttrSplitting' ::  [entry]      -- ^ select from
                            -> Set AttributeName  -- ^ except given attributes
                            -> [(([AttrValSet], Float), [[(AttributeContainer, Int)]])] -- ^ sorted splitting
