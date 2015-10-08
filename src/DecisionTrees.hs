--{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- Module      :  DecisionTrees
-- License     :  MIT
-- Stability   :  dev


module DecisionTrees (

  Decision(..)
, DecisionTree

, listChildren
, decision2Tree

, buildDecisionTree

) where


import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ( (&&&), second )

import DecisionTrees.Learning


data Decision entity clazz
  -- | A decision tree node.
  = forall cond . Show cond =>
    DecisionStep { prepare :: entity -> cond                     -- ^ extract a 'cond' for further selection
                 , select  :: Map [cond] (Decision entity clazz) -- ^ the next nodes; the selection is based on
                                                                 --   whether a 'cond' is contained in the key.
                 , describePrepare :: String                     -- ^ a description for 'prepare' transformation.
                 }
  -- | A decision tree leaf.
  | Decision { classification :: Map clazz Int  -- ^ classes and corresponding entities count.
             }


instance (Show decision) =>
    Show (Decision entity decision)
        where show (DecisionStep _ sel dp) = dp ++ "\t==>\t" ++ show (Map.keys sel)
              show (Decision d)            = show d

type DecisionTree entity decision = Tree (Decision entity decision)

-- | List children nodes.
listChildren :: Decision e d -> [Decision e d]
listChildren (DecisionStep _ sel _) = Map.elems sel
listChildren (Decision _)           = []

-- | converts a 'Decision' into ('Data.Tree' 'Decision')
decision2Tree = unfoldTree (id &&& listChildren)


-----------------------------------------------------------------------------
-- | build a decision tree, using the imported instance of 'TreeBranching'
buildDecisionTree :: (Entry entry, TreeBranching entry) =>
    [entry] -> Decision entry AttributeContainer

buildDecisionTree entries =
    case finishedSplitting entries of Just clazz -> Decision (Map.fromList clazz)
                                      _          -> buildStep
    where buildStep = DecisionStep prepare (Map.fromList next) (show bestAttr)
          (best, _) = selectBestAttrSplitting entries
          splitted  = splitEntries entries best
          bestAttr  = fst . head $ best
          prepare   = attrByName bestAttr
          next = do (attrVS, entries') <- splitted
                    let attrCs = Set.toList . snd $ attrVS
                    return (attrCs, buildDecisionTree entries')


