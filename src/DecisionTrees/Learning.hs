-- |
--
-- Module      :  DecisionTrees.Learning
-- License     :  MIT
-- Stability   :  dev

module DecisionTrees.Learning (

  Attribute(..)
, AttributeContainer(..)
, AttributeName
, attrName

, Entry(..)
, TreeBranching(..)

) where


import Data.Set (Set)




newtype AttributeName = AttrName String deriving (Eq, Ord)

type PossibleDiscreteDomain attr = [[attr]]
type AttrValSet = (AttributeName, Set AttributeContainer)

instance Show AttributeName where show (AttrName name) = name





class (Show attr) =>
    Attribute attr where
        possibleDiscreteDomains :: attr -> [PossibleDiscreteDomain attr]
        attributeName           :: attr -> AttributeName

data AttributeContainer = forall attr . (Attribute attr) => Attr attr

instance Show AttributeContainer where show (Attr attr) = show attr

instance Eq AttributeContainer where
    x == y = attrName x == attrName y && show x == show y

instance Ord AttributeContainer where
    x <= y = f x <= f y
        where f x = map ($ x) [show . attrName, show]

attrName (Attr attr) = attributeName attr
--attr2str (Attr attr) = show attr




class Entry entry where
    -- | List the attributes, except class
    listAttributes :: entry -> [AttributeContainer]
    getClass       :: entry -> AttributeContainer
    attrByName     :: AttributeName -> entry -> AttributeContainer


class TreeBranching entry where
    selectBestAttrSplitting :: [entry] -> ([AttrValSet], Float)
    splitEntries            :: [entry] -> [AttrValSet] -> [(AttrValSet, [entry])]
    finishedSplitting       :: [entry] -> Maybe [(AttributeContainer, Int)]


