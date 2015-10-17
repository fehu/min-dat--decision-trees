-----------------------------------------------------------------------------
--
-- Module      :  DecisionTrees.TreeBranching.Debug
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

module DecisionTrees.TreeBranching.Debug (
  TreeBranchingDebug(..)
) where

import DecisionTrees.TreeBranching
import DecisionTrees.Definitions
import Data.Set (Set)

class (TreeBranching entry) =>
    TreeBranchingDebug entry where
        selectBestAttrSplitting' ::  [entry]      -- ^ select from
                            -> Set AttributeName  -- ^ except given attributes
                            -> [(([AttrValSet], Float), [[(AttributeContainer, Int)]])] -- ^ sorted splitting
