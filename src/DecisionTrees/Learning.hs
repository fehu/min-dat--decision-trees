-- |
--
-- Module      :  DecisionTrees.Learning
-- License     :  MIT
-- Stability   :  dev

module DecisionTrees.Learning (

  Attribute(..)
, AttributeContainer(..)
, AttributeName(..)
, AttrValSet

, Entry(..)
, TreeBranching(..)

) where


import Data.Set (Set)
import Data.Typeable




newtype AttributeName = AttrName String deriving (Eq, Ord)

type PossibleDiscreteDomain attr = [[attr]]
type AttrValSet = (AttributeName, Set AttributeContainer)

instance Show AttributeName where show (AttrName name) = name





class (Show attr) =>
    Attribute attr where
        possibleDiscreteDomains :: attr -> [PossibleDiscreteDomain attr]
        attributeName           :: attr -> AttributeName

-- http://stackoverflow.com/questions/13015949/testing-equality-between-two-heterogeneous-values
data AttributeContainer = forall attr . (Attribute attr, Typeable attr, Ord attr) => Attr attr

instance Show AttributeContainer where show (Attr attr) = show attr

instance Eq AttributeContainer where
    (Attr x) == (Attr y) =
        case cast y of Just y' -> x == y'
                       Nothing -> False

instance Ord AttributeContainer where
    (Attr x) `compare` (Attr y) =
        case cast y of Just y' -> x `compare` y'
                       Nothing -> let c = attributeName x `compare` attributeName y
                                  in case c of EQ -> error "EQ"
                                               _  -> c

attrName       (Attr attr) = attributeName attr




class (Show entry) =>
    Entry entry where
        -- | List the attributes, except class
        listAttributes :: entry -> [AttributeContainer]
        getClass       :: entry -> AttributeContainer
        classDomain    :: entry -> Set AttributeContainer
        attrByName     :: AttributeName -> entry -> AttributeContainer


class TreeBranching entry where
    -- | select best attributes splitting
    selectBestAttrSplitting :: [entry]               -- ^ select from
                            -> Set AttributeName     -- ^ except given attributes
                            -> ([AttrValSet], Float) -- ^ best splitting
    splitEntries            :: [entry] -> [AttrValSet] -> [(AttrValSet, [entry])]
    finishedSplitting       :: [entry] -> Maybe [(AttributeContainer, Int)]



