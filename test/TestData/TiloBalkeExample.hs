-- |
--
-- Module      :  TestData.TiloBalkeExample
-- Description :  An example from Tilo Balke's presentation.
-- Copyright   :  Wolf-Tilo Balke, Silviu Homoceanu. Institut für Informationssysteme
--                      Technische Universität Braunschweig <http://www.ifis.cs.tu-bs.de>
-- License     :  AllRightsReserved
-- Stability   :  dev
--
-- An example from Tilo Balke's presentation.

module TestData.TiloBalkeExample (

  Entry(..)
, CreditRating(..)

, testData
, expectedDecision

--, splitAge
) where

import DecisionTrees
import qualified DecisionTrees.Learning as L

import qualified Data.Map as Map

newtype Age = Age Int                deriving (Show, Eq, Ord)
newtype Income = Income String       deriving (Show, Eq, Ord)
newtype Student = Student Bool       deriving (Show, Eq, Ord)
data CreditRating = Fair | Excellent deriving (Show, Eq, Ord)
data BuysComputer = Yes  | No        deriving (Show, Eq, Ord)

data Entry = Entry{ age         :: Age
                  , income      :: Income
                  , student     :: Student
                  , credRating  :: CreditRating
                  , buysComp    :: BuysComputer
                  }
              deriving (Show, Eq)

instance L.Attribute Age where
    possibleDiscreteDomains _ = [
       [ map Age [0..30], map Age [31..40], map Age [40..] ] -- TODO not the best implementation
     ]
    attributeName _ = L.AttrName "age"

instance L.Attribute Income where
    possibleDiscreteDomains _ = [ [ [Income "high"], [Income "medium"], [Income "low"] ] ]
    attributeName _ = L.AttrName "income"

instance L.Attribute Student where
    possibleDiscreteDomains _ = [ [ [Student True], [Student False] ] ]
    attributeName _ = L.AttrName "student"

instance L.Attribute CreditRating where
    possibleDiscreteDomains _ = [ [ [Fair], [Excellent] ] ]
    attributeName _ = L.AttrName "credit rating"

instance L.Attribute BuysComputer where
    possibleDiscreteDomains _ = [ [ [Yes], [No] ] ]
    attributeName _ = L.AttrName "buys computer"

instance L.Entry Entry where
    -- entry -> [AttributeContainer]
    listAttributes entry = map ($ entry) [
       L.Attr . age
     , L.Attr . income
     , L.Attr . student
     , L.Attr . credRating
     ]
    getClass = L.Attr . buysComp
    attrByName (L.AttrName name) =
        case name of "age"    -> L.Attr . age
                     "income" -> L.Attr . income

newEntry age income student= Entry (Age age) (Income income) (Student student)


testData :: [Entry]
testData = [
   newEntry 25 "high"   False Fair      No
 , newEntry 27 "high"   False Excellent No
 , newEntry 35 "high"   False Fair      Yes
 , newEntry 42 "medium" False Excellent Yes
 , newEntry 52 "low"    True  Excellent Yes
 , newEntry 45 "low"    True  Fair      No
 , newEntry 31 "low"    True  Excellent Yes
 , newEntry 29 "medium" False Fair      No
 , newEntry 22 "low"    True  Fair      Yes
 , newEntry 42 "medium" True  Excellent Yes
 , newEntry 30 "medium" True  Excellent Yes
 , newEntry 36 "medium" False Excellent Yes
 , newEntry 33 "high"   True  Fair      Yes
 , newEntry 50 "medium" False Fair      No
 ]

splitAge (Age age) | age <= 30 = "<=30"
                   | age >  40 = ">40"
                   | otherwise = "31..40"



expectedDecision :: Decision Entry Bool
expectedDecision =
    DecisionStep { prepare = splitAge . age
                 , describePrepare = "age"
                 , select = Map.fromList [ (["<=30"],   treeAgeYoung)
                                         , (["31..40"], treeAgeMiddle)
                                         , ([">40"],    treeAgeSenior)
                                         ]
                 }


treeAgeYoung  = DecisionStep { prepare = student
                             , describePrepare = "student?"
                             , select = Map.fromList [ ([Student True],  treeYoungStudent)
                                                     , ([Student False], treeYoungNotStudent)
                                                     ]
                             }
treeYoungStudent    = Decision { classification = Map.fromList [(False, 3)]  }
treeYoungNotStudent = Decision { classification = Map.fromList [(True, 2)] }


treeAgeMiddle = Decision { classification = Map.fromList [(True, 4)] }


treeAgeSenior = DecisionStep { prepare = credRating
                             , describePrepare = "credit rating"
                             , select = Map.fromList [ ([Fair],      treeSeniorFair)
                                                     , ([Excellent], treeSeniorExcellent)
                                                     ]
                             }
treeSeniorFair      = Decision $ Map.fromList [(False, 2)]
treeSeniorExcellent = Decision $ Map.fromList [(True, 3)]

