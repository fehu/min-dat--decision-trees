-- |
--
-- Module      :  DecisionTrees.Learning
-- License     :  MIT
-- Stability   :  dev

module DecisionTrees.Learning (

  Attribute(..)
, AttributeContainer(..)
, attrName

, Entry(..)
, TreeBranching(..)

) where


class (Show attr) =>
    Attribute attr where
        possibleDiscreteDomains :: [[attr]]
        attributeName           :: attr -> String

data AttributeContainer = forall attr . (Attribute attr) => Attr attr

instance Show AttributeContainer where show (Attr attr) = show attr

instance Eq AttributeContainer where
    x == y = attrName x == attrName y && show x == show y

instance Ord AttributeContainer where
    x <= y = f x <= f y
        where f x = map ($ x) [attrName, show]

attrName (Attr attr) = attributeName attr
--attr2str (Attr attr) = show attr


class Entry entry where
    -- | List the attributes, except class
    listAttributes :: entry -> [AttributeContainer]
    getClass       :: entry -> AttributeContainer
    attrByName     :: String -> entry -> AttributeContainer

--class AttributesSplitting attr where
--    splitAttribute :: [attr]

class TreeBranching entry where
    selectBestAttrSplitting :: [entry] -> ([AttributeContainer], Float)
    splitEntries            :: [entry] -> [AttributeContainer] -> [(AttributeContainer, [entry])]
    finishedSplitting       :: [entry] -> Maybe AttributeContainer


