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
, buildDecisionTreeIterative

, classifyWithDecisionTree

) where


import Data.Tree
import Data.Map (Map)
import Data.Set (Set)
import Data.List(intercalate, (\\))
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ( (&&&), second )

import DecisionTrees.Definitions
import DecisionTrees.TreeBranching
import DecisionTrees.Utils


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
  | Undefined{ describeSelect :: Maybe String }


instance (Show decision) =>
    Show (Decision entity decision)
        where show (DecisionStep _ sel dp ds) = maybe "" (++" ==> ") ds ++ dp
              show (Decision d ds)            = maybe "" (++" ==> ") ds
                                                 ++ ( intercalate ", "
                                                    . map (\(c,i) -> show c ++ ": " ++ show i)
                                                    . Map.toAscList
                                                    $ d
                                                    )
              show (Undefined ds)             = "? " ++ fromMaybe "" ds ++ " ?"

type DecisionTree entity decision = Tree (Decision entity decision)

-- | list children nodes.
listChildren :: Decision e d -> [Decision e d]
listChildren (DecisionStep _ sel _ _) = Map.elems sel
listChildren (Decision _ _)           = []

-- | converts a 'Decision' into 'Data.Tree'
decision2Tree f = unfoldTree (f &&& listChildren)


-- | classify an entry using a "Decision".
classifyWithDecisionTree :: Entry entry =>
     Decision entry AttributeContainer -- ^ a decision tree
     -> entry                          -- ^ an entry to classify
     -> AttributeContainer             -- ^ classification result

classifyWithDecisionTree (DecisionStep prep sel dp _) entry =
    case next of [(_, nxt)] -> classifyWithDecisionTree nxt entry
                 _          -> error $ "not found " ++ dp ++ " in " ++ show entry
    where esel = prep entry
          next = Map.toList $ Map.filterWithKey (\k _ -> esel `elem` k) sel

classifyWithDecisionTree (Decision clazzMap _) _ = fst $ Map.findMax clazzMap


-----------------------------------------------------------------------------
-- | build a decision tree, based on the /learning/ entries,
--   using the imported instance of 'TreeBranching'.
--   The /test/ entries are used to assess tree's quality;
--   failed examples are added to the /learning/ set and the process is rerun.
buildDecisionTreeIterative :: (Eq entry, Entry entry, TreeBranching entry) =>
                              (?clazz :: ClassDescriptor) =>
        [entry]     -- ^ learning entries
     -> [entry]     -- ^ test entries
     -> IO (Decision entry AttributeContainer)  -- ^ the decision tree

buildDecisionTreeIterative learnEntries testEntries = do
    dtree <- buildDecisionTree learnEntries
    let failTst = filter (\e -> classifyWithDecisionTree dtree e /= getClass e) testEntries
    if null failTst
        then return dtree
        else do let newLearn = learnEntries ++ failTst
                let newTest  = testEntries \\ failTst
                buildDecisionTreeIterative newLearn newTest


-----------------------------------------------------------------------------
-- | build a decision tree, based on the given entries,
--   using the imported instance of 'TreeBranching'.
buildDecisionTree :: (Entry entry, TreeBranching entry) => (?clazz :: ClassDescriptor) =>
    [entry] -> IO (Decision entry AttributeContainer)

buildDecisionTree entries = buildDecisionTree' entries' Set.empty Nothing
    where Class c = ?clazz
          entries' = filter (hasAttribute $ AttrName c) entries



buildDecisionTree' :: (Entry entry, TreeBranching entry) => (?clazz :: ClassDescriptor) =>
    [entry] -> Set AttributeName -> Maybe String -> IO (Decision entry AttributeContainer)

buildDecisionTree' [] _ ds = return $ Undefined ds
buildDecisionTree' entries ignore selDescr
    | null best = return $ Decision classes selDescr
--    | totalAttributes (head entries) == Set.size ignore + 1 = return $ Decision classes selDescr
--    | Set.fromList . map $ entries == ignore = undefined
    | otherwise =
    case finishedSplitting entries of Just clazz -> return $ Decision (Map.fromList clazz) selDescr
                                      _          -> buildStep
    where classes   = Map.fromList $ sortingGroupBy getClass length entries
          buildStep = do nxt <- sequence next
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


