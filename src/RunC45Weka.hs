-----------------------------------------------------------------------------
--
-- Module      :  RunC45Weka
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

module RunC45Weka (
  WekaVal(..)
, WekaAttributeRepr(..)
, GenericEntry(..)

, run
, runIterative
, drawDecisionTree

) where

import DecisionTrees
import DecisionTrees.Definitions
import DecisionTrees.TreeBranching
import DecisionTrees.C45
import DecisionTrees.Utils
import WekaData
import WekaData.Show.Name


import Control.Arrow
import Data.Typeable
import Data.Map.Strict (Map)
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree
import GHC.Float

import Data.Random
import Data.Random.Sample
import qualified Data.Random.Extras as RE

-----------------------------------------------------------------------------


data WekaVal = WekaNumVal Float
             | WekaNomVal String
             deriving (Typeable, Eq, Ord)

instance Show WekaVal where
    show (WekaNumVal f) = show f
    show (WekaNomVal s) = show s


data GenericEntry = GenericEntry (Map WekaDataAttribute WekaVal)
                                 (WekaDataAttribute, WekaVal)
                deriving (Show, Eq)


newtype WekaAttributeRepr = WekaAttributeRepr (WekaDataAttribute, WekaVal)
                          deriving (Typeable, Eq, Ord)

instance Show WekaAttributeRepr where
    show (WekaAttributeRepr (attr, val)) = show attr ++ "=" ++ show val


buildAttr attr = WekaAttributeRepr . (const attr &&& WekaNomVal)
mkAttr = Attr . WekaAttributeRepr

instance Attribute WekaAttributeRepr where
 -- possibleDiscreteDomains :: attr -> [PossibleDiscreteDomain attr]
    possibleDiscreteDomains (WekaAttributeRepr (attr@(WekaAttrNom _ domain), _)) =
        flists (buildAttr attr) fsubs
            where fsubs = fullSubsets . Set.fromList $ domain
                  fl f  = map f . Set.toList
                  flists f = fl (fl (fl f))
 -- attributeName :: attr -> AttributeName
    attributeName (WekaAttributeRepr(attr, _)) = AttrName $ wekaAttributeName attr


instance Entry GenericEntry where
 -- listAttributes :: entry -> [AttributeContainer]
    listAttributes (GenericEntry dta _) = map mkAttr $ Map.toAscList dta

 -- getClass :: entry -> AttributeContainer
    getClass (GenericEntry _ clazz) = mkAttr clazz

 -- classDomain :: entry -> Set AttributeContainer
    classDomain (GenericEntry _ (attr@(WekaAttrNom _ domain), _)) =
        Set.fromList . map (Attr . buildAttr attr) $ domain

 -- attrByName :: AttributeName -> entry -> AttributeContainer
    attrByName (AttrName name) (GenericEntry dta _) =
        mkAttr $ findInMapWithAttr name dta



-----------------------------------------------------------------------------

type Filename = String

run :: Filename -> IO (Decision GenericEntry AttributeContainer)
run filename = do entries <- getEntries filename
                  buildDecisionTree entries


mkEntry :: [WekaDataAttribute] -> [String] -> GenericEntry
mkEntry attrs vals = GenericEntry vmap clazz
    where vmap  = Map.fromList $ zip (init attrs) (map WekaNomVal $ init vals)
          clazz = (last attrs, WekaNomVal $ last vals)

getEntries filename = do
    RawWekaData name attrs dta <- readWekaData filename
    return $ map (mkEntry attrs) dta


-----------------------------------------------------------------------------

runIterative :: Filename    -- ^ file name
             -> Float       -- ^ the percent of /test/ entries
             -> IO (Decision GenericEntry AttributeContainer) -- ^ the decision tree


runIterative filename testPercent = do
    entries <- getEntries filename
    let testEntriesCount = float2Int $ int2Float (length entries) * testPercent
    testEntries <- runRVar (RE.sample testEntriesCount entries) StdRandom
    let learnEntries = entries \\ testEntries
    buildDecisionTreeIterative learnEntries testEntries

-----------------------------------------------------------------------------

drawDecisionTree :: Decision GenericEntry AttributeContainer -> IO()
drawDecisionTree res = do let tr = decision2Tree show res
                          putStrLn $ Tree.drawTree tr


