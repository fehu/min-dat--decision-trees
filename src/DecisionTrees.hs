--{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- Module      :  DecisionTrees
-- License     :  MIT
-- Stability   :  dev


module DecisionTrees (

  Decision(..)
--, DecisionTree

--, DecisionBuilding(..)
--, unfoldDecision
--, unfoldDecisions
) where


import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ( (&&&), second )

import DecisionTrees.Learning

data Decision entity decision = forall cond . Show cond =>
                                DecisionStep { prepare :: entity -> cond
                                             , select  :: Map cond (Decision entity decision)
                                             , describePrepare :: String
                                             }
                              | Decision { decision :: decision }

instance (Show decision) =>
    Show (Decision entity decision)
        where show (DecisionStep _ sel dp) = dp ++ "\t==>\t" ++ show (Map.keys sel)
              show (Decision d)            = show d

type DecisionTree entity decision = Tree (Decision entity decision)

listChildren :: Decision e d -> [Decision e d]
listChildren (DecisionStep _ sel _) = Map.elems sel
listChildren (Decision _)           = []

decision2Tree = unfoldTree (id &&& listChildren)

-----------------------------------------------------------------------------


buildDecisionTree :: (Entry entry, TreeBranching entry) =>
    [entry] -> Decision entry AttributeContainer

buildDecisionTree entries =
    case finishedSplitting entries of Just clazz -> Decision clazz
                                      _          -> buildStep
    where buildStep = DecisionStep prepare (Map.fromList next) bestAttr
          (best, _) = selectBestAttrSplitting entries
          splitted  = splitEntries entries best
          bestAttr  = attrName (head best)
          prepare   = attrByName bestAttr
          next = do (attrC, entries') <- splitted
                    return (attrC, buildDecisionTree entries')



--data DecisionBuilding e d = forall cond . (Show cond, Ord cond) =>
--    DecisionBuilding { buildThis :: (e -> cond, String)
--                     , buildNext :: [(cond, DecisionBuilding e d)]
--                     }
--  | DecisionBuilt d
--
--unfoldDecision :: (DecisionBuilding e d -> DecisionBuilding e d) -> DecisionBuilding e d -> Decision e d
--unfoldDecision f b = case f b of DecisionBuilt d              -> Decision d
--                                 DecisionBuilding bThis bNext -> DecisionStep (fst bThis) next (snd bThis)
--                                    where next = unfoldDecisions f bNext
--
--unfoldDecisions :: (Ord cond) =>
--    (DecisionBuilding e d -> DecisionBuilding e d) -> [(cond, DecisionBuilding e d)] -> Map cond (Decision e d)
--unfoldDecisions f bs = Map.fromList $ map (second (unfoldDecision f)) bs

-- :: (b -> (a, [b])) -> [b] -> Forest a
-- f = map (unfoldTree f)

--unfoldDecision :: (b -> (Decision e d, [b])) -> b -> Decision e d
--unfoldDecision f b = undefined
--    where (decision, childrenSeeds) = f b

--unfoldDecision f b = let (a, bs) = f b in Node a (unfoldForest f bs)

--d :: Decision String String
--d = DecisionStep { prepare = length
--                 , select "a" = undefined
--                 }



--data Tree entiry prev this next = Root (entiry -> this) (this -> Bool) [Tree entity this next ?]
