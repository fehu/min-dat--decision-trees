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
class TreeBranching entry conf where
    -- | select best attributes splitting.
    selectBestAttrSplitting :: ( ?clazz  :: ClassDescriptor
                               , ?config :: conf ) =>
                               [entry]               -- ^ select from
                            -> Set AttributeName     -- ^ except given attributes
                            -> ([AttrValSet], Float) -- ^ best splitting
    -- | group entries, based on the given 'AttrValSet's.
    splitEntries            :: (?config :: conf) => [entry] -> [AttrValSet] -> [(AttrValSet, [entry])]
    -- | stop condition for branching.
    finishedSplitting       :: ( ?clazz :: ClassDescriptor
                               , ?config :: conf ) =>
                               [entry]
                            -> Maybe [(AttributeContainer, Int)]



