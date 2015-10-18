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
import Data.Set (Set)
import Data.List(intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ( (&&&), second )

import DecisionTrees.Definitions
import DecisionTrees.TreeBranching


data Decision entity clazz
  -- | A decision tree node.
  = forall cond . (Show cond, Ord cond) =>
    DecisionStep { prepare :: entity -> cond                     -- ^ extract a 'cond' for further selection
                 , select  :: Map [cond] (Decision entity clazz) -- ^ the next nodes; the selection is based on
                                                                 --   whether a 'cond' is contained in the key.
                 , describePrepare :: String                     -- ^ a description for 'prepare' transformation.
                 , describeSelect  :: Maybe String
                 }
  -- | A decision tree leaf.
  | Decision { classification :: Map clazz Int  -- ^ classes and corresponding entities count.
             , describeSelect :: Maybe String
             }


instance (Show decision) =>
    Show (Decision entity decision)
        where show (DecisionStep _ sel dp ds) = maybe "" (++" ==> ") ds ++ dp
              show (Decision d ds)            = maybe "" (++" ==> ") ds
                                                 ++ ( intercalate ", "
                                                    . map (\(c,i) -> show c ++ ": " ++ show i)
                                                    . Map.toAscList
                                                    $ d
                                                    )

type DecisionTree entity decision = Tree (Decision entity decision)

-- | List children nodes.
listChildren :: Decision e d -> [Decision e d]
listChildren (DecisionStep _ sel _ _) = Map.elems sel
listChildren (Decision _ _)           = []

-- | converts a 'Decision' into 'Data.Tree'
decision2Tree f = unfoldTree (f &&& listChildren)


-----------------------------------------------------------------------------
-- | build a decision tree, using the imported instance of 'TreeBranching'
buildDecisionTree :: (Entry entry, TreeBranching entry) =>
    [entry] -> IO (Decision entry AttributeContainer)

buildDecisionTree entries = buildDecisionTree' entries Set.empty Nothing

buildDecisionTree' :: (Entry entry, TreeBranching entry) =>
    [entry] -> Set AttributeName -> Maybe String -> IO (Decision entry AttributeContainer)

--buildDecisionTree' entries 10 = return $ Decision Map.empty

buildDecisionTree' entries ignore selDescr =
    case finishedSplitting entries of Just clazz -> return $ Decision (Map.fromList clazz) selDescr
                                      _          -> buildStep
    where buildStep = do nxt <- sequence next
--                         putStrLn "buildStep"
--                         putStrLn $ "best = " ++ show best
--                         putStrLn $ "v = " ++ show v ++ "\n"
--                         putStrLn $ "splitted = " ++ show splitted
                         return $ DecisionStep prepare (Map.fromList nxt) (show bestAttr) selDescr
          (best, v) = selectBestAttrSplitting entries ignore
          splitted  = splitEntries entries best
          bestAttr  = case best of []        -> error $ "empty best: " ++ show best
                                   (ba, _):_ -> ba
          prepare   = attrByName bestAttr
          next = do (attrVS, entries') <- splitted
                    let attrCs = Set.toList . snd $ attrVS
                    let nextSel = Just $ intercalate ", " . map show $ attrCs
                    return $ do tr <- buildDecisionTree' entries' (Set.insert bestAttr ignore) nextSel
--                                putStrLn $ "next: attrVS = " ++ show attrVS
--                                putStrLn $ "entries' = " ++ show entries'
                                return (attrCs, tr)


