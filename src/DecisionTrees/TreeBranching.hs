-- |
--
-- Module      :  DecisionTrees.TreeBranching
-- License     :  MIT
-- Stability   :  dev

module DecisionTrees.TreeBranching (

  TreeBranching(..)

) where

import DecisionTrees.Definitions

import Data.Set (Set)




class TreeBranching entry where
    -- | select best attributes splitting
    selectBestAttrSplitting :: [entry]              -- ^ select from
                            -> Set AttributeName    -- ^ except given attributes
                            -> (AttrValSet, Float)  -- ^ best splitting
    splitEntries            :: [entry] -> AttrValSet -> [entry]
    finishedSplitting       :: [entry] -> Maybe [(AttributeContainer, Int)]



