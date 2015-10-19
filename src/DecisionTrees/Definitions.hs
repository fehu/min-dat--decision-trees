-- |
--
-- Module      :  DecisionTrees.Definitions
-- Description :  Some definitions for DecisionTrees.
-- License     :  MIT
-- Stability   :  dev
--
-- Defines 'Entry' and 'Attribute'.


module DecisionTrees.Definitions (

  Attribute(..)
, AttributeContainer(..)
, AttributeName(..)
, AttrValSet

, Entry(..)

) where


import Data.Set (Set)
import Data.Typeable


-----------------------------------------------------------------------------
-- | An isomorphic container for attribute name.
newtype AttributeName = AttrName String deriving (Eq, Ord)

type PossibleDiscreteDomain attr = [[attr]]
-- | A set of values of an attribute.
type AttrValSet = (AttributeName, Set AttributeContainer)

instance Show AttributeName where show (AttrName name) = name

-----------------------------------------------------------------------------


-- | A typeclass, describing attribute.
class (Show attr) =>
    Attribute attr where
        -- | all possible discrete domains of an attribute
        possibleDiscreteDomains :: attr -> [PossibleDiscreteDomain attr]
        -- | the name of an attribute.
        attributeName           :: attr -> AttributeName

-- | Heterogeneous container for an attribute. See
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

-----------------------------------------------------------------------------


-- | A typeclass, describing data entries, 
--   that consist of attributes and class.
class (Show entry) =>
    Entry entry where
        -- | List the attributes, except class
        listAttributes :: entry -> [AttributeContainer]
        getClass       :: entry -> AttributeContainer
        classDomain    :: entry -> Set AttributeContainer
        attrByName     :: AttributeName -> entry -> AttributeContainer




