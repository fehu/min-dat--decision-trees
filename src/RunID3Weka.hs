{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  RunID3Weka
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

module RunID3Weka (

  run
, runIterative
, drawDecisionTree

, FinishedSplittingThreshold(..)

) where

import DecisionTrees
import DecisionTrees.Definitions
import DecisionTrees.TreeBranching
import DecisionTrees.ID3
import DecisionTrees.Utils
import WekaData
import WekaData.Show.Name


import Control.Arrow
import Data.Typeable
import Data.Map.Strict (Map)
import Data.List ((\\))
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import GHC.Float

import Data.Random
import Data.Random.Sample
import qualified Data.Random.Extras as RE

-----------------------------------------------------------------------------

buildAttr attr = uncurry WVal . (const attr &&& id)

instance Attribute WekaVal where
 -- possibleDiscreteDomains :: attr -> [PossibleDiscreteDomain attr]
    possibleDiscreteDomains (WVal attr@(WekaAttrNom _ domain) _) =
        flists (buildAttr attr) fsubs
            where fsubs = fullSubsets . Set.fromList $ domain
                  fl f  = map f . Set.toList
                  flists f = fl (fl (fl f))
 -- attributeName :: attr -> AttributeName
    attributeName (WVal attr _) = AttrName $ wekaAttributeName attr


separateClass :: (?clazz :: ClassDescriptor) => WekaEntry -> (WekaVal, [WekaVal])
separateClass e@(WEntry set) = maybe err f $ lookupWValInSet c set
    where Class c = ?clazz
          f = id &&& (Set.toList . (`Set.delete` set))
          err = error $ "Class attribute '" ++ c ++ "' not found in " ++ show e

instance Entry WekaEntry where

 -- listAttributes :: entry -> [AttributeContainer]
    listAttributes  = map Attr . snd . separateClass

 -- getClass :: entry -> AttributeContainer
    getClass = Attr . fst . separateClass

 -- classDomain :: entry -> Set AttributeContainer
    classDomain = Set.fromList . f . fst . separateClass
                where f (WVal attr@(WekaAttrNom _ domain) _) =
                        map (Attr . WVal attr) domain

 -- attrByName :: AttributeName -> entry -> AttributeContainer
    attrByName (AttrName name) e@(WEntry dta) =
        maybe err Attr $ lookupWValInSet name dta
        where err = error $ "Attribute '" ++ name
                 ++ "' not found in " ++ show e

    hasAttribute (AttrName name) (WEntry set) = isJust $ lookupWValInSet name set


-----------------------------------------------------------------------------

type Filename = String

run :: Filename -- ^ file name
    -> String   -- ^ class name
    -> FinishedSplittingThreshold
    -> IO (Decision WekaEntry AttributeContainer) -- ^ the decision tree
run filename classname fsThreshold =
    do entries <- getEntries filename
       let ?clazz = Class classname
       let ?config = fsThreshold
       buildDecisionTree entries


getEntries filename = do
    raw@(RawWekaData name attrs dta) <- readWekaData filename
    return $ wekaData2Sparse raw


-----------------------------------------------------------------------------

runIterative :: Filename    -- ^ file name
             -> String      -- ^ class name
             -> FinishedSplittingThreshold
             -> Float       -- ^ the percent of /test/ entries
             -> IO (Decision WekaEntry AttributeContainer) -- ^ the decision tree


runIterative filename classname fsThreshold testPercent = do
    entries <- getEntries filename
    let testEntriesCount = float2Int $ int2Float (length entries) * testPercent
    testEntries <- runRVar (RE.sample testEntriesCount entries) StdRandom
    let learnEntries = entries \\ testEntries
    let ?clazz = Class classname
    let ?config = fsThreshold
    buildDecisionTreeIterative learnEntries testEntries
    undefined

-----------------------------------------------------------------------------

drawDecisionTree :: Decision WekaEntry AttributeContainer -> IO()
drawDecisionTree res = do let tr = decision2Tree show res
                          putStrLn $ Tree.drawTree tr


