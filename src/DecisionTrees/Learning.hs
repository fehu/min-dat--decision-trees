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


newtype AttributeName = AttrName String deriving (Eq, Ord)

instance Show AttributeName where show (AttrName name) = name

class (Show attr) =>
    Attribute attr where
        possibleDiscreteDomains :: attr -> [[attr]]
        attributeName           :: attr -> AttributeName

data AttributeContainer = forall attr . (Attribute attr) => Attr attr

instance Show AttributeContainer where show (Attr attr) = show attr

instance Eq AttributeContainer where
    x == y = attrName x == attrName y && show x == show y

instance Ord AttributeContainer where
    x <= y = f x <= f y
        where f x = map ($ x) [show . attrName, show]

attrName (Attr attr) = show . attributeName $ attr
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
--    splitEntries            :: [entry] -> [AttributeContainer] -> [(AttributeContainer, [entry])]
    splitEntries            :: [entry] -> [AttributeContainer] -> [(AttributeContainer, [entry])]
    finishedSplitting       :: [entry] -> Maybe AttributeContainer


