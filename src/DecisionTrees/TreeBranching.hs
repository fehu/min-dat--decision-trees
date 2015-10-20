-- |
--
-- Module      :  DecisionTrees.TreeBranching
-- License     :  MIT
-- Description :  An abstraction for building decision trees.
-- Stability   :  dev
--
-- An abstraction for building decision trees.

module DecisionTrees.TreeBranching (

  TreeBranching(..)

) where

import DecisionTrees.Definitions

import Data.Set (Set)



-- | An abstraction for building decision trees.
class TreeBranching entry where
    -- | select best attributes splitting.
    selectBestAttrSplitting :: [entry]               -- ^ select from
                            -> Set AttributeName     -- ^ except given attributes
                            -> ([AttrValSet], Float) -- ^ best splitting
    -- | group entries, based on the given 'AttrValSet's.
    splitEntries            :: [entry] -> [AttrValSet] -> [(AttrValSet, [entry])]
    -- | stop condition for branching.
    finishedSplitting       :: [entry] -> Maybe [(AttributeContainer, Int)]



