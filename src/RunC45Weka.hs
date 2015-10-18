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

) where

import DecisionTrees
import DecisionTrees.Definitions
import DecisionTrees.TreeBranching
import DecisionTrees.C45
import WekaData


import Control.Arrow
import Data.Typeable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import DecisionTrees.Utils

-----------------------------------------------------------------------------


data WekaVal = WekaNumVal Float
             | WekaNomVal String
             deriving (Typeable, Eq, Ord)

instance Show WekaVal where
    show (WekaNumVal f) = show f
    show (WekaNomVal s) = show s


data GenericEntry = GenericEntry (Map WekaDataAttribute WekaVal)
                                 (WekaDataAttribute, WekaVal)
                deriving Show

type WekaAttributeRepr = (WekaDataAttribute, WekaVal)


buildAttr attr = const attr &&& WekaNomVal


instance Attribute (WekaDataAttribute, WekaVal) where
 -- possibleDiscreteDomains :: attr -> [PossibleDiscreteDomain attr]
    possibleDiscreteDomains (attr@(WekaAttrNom _ domain), _) =
        flists (buildAttr attr) fsubs
            where fsubs = fullSubsets . Set.fromList $ domain
                  fl f  = map f . Set.toList
                  flists f = fl (fl (fl f))
 -- attributeName :: attr -> AttributeName
    attributeName (attr, _) = AttrName $ wekaAttributeName attr


instance Entry GenericEntry where
 -- listAttributes :: entry -> [AttributeContainer]
    listAttributes (GenericEntry dta _) = map Attr $ Map.toAscList dta

 -- getClass :: entry -> AttributeContainer
    getClass (GenericEntry _ clazz) = Attr clazz

 -- classDomain :: entry -> Set AttributeContainer
    classDomain (GenericEntry _ (attr@(WekaAttrNom _ domain), _)) =
        Set.fromList . map (Attr . buildAttr attr) $ domain

 -- attrByName :: AttributeName -> entry -> AttributeContainer
    attrByName (AttrName name) (GenericEntry dta _) = Attr $ findInMapWithAttr name dta



-----------------------------------------------------------------------------

type Filename = String

run :: Filename -> IO (Decision entry AttributeContainer)

run filename = do rawData <- readWekaData filename

                  undefined
