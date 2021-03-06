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

-- | An abstraction for debugging decision trees building process.
class (TreeBranching entry conf) =>
    TreeBranchingDebug entry conf where
        -- | best attributes for splitting is descending order.
        selectBestAttrSplitting' :: (?clazz :: ClassDescriptor, ?config :: conf ) =>
                               [entry]      -- ^ select from
                            -> Set AttributeName  -- ^ except given attributes
                            -> [(([AttrValSet], Float), [[(AttributeContainer, Int)]])] -- ^ sorted splitting
