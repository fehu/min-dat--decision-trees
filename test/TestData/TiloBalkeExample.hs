{-# LANGUAGE DeriveDataTypeable #-}
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
, Age(..)
, Income(..)
, Student(..)
, CreditRating(..)
, BuysComputer(..)

, testData
, expectedDecision

--, splitAge
) where

import DecisionTrees
import qualified DecisionTrees.Learning as L

import qualified Data.Map as Map
import Control.Applicative
import Data.Typeable

--newtype Age = Age Int                deriving (Show, Eq, Ord)
data Age = Age Int | AgeRange (Maybe Int) (Maybe Int) deriving Typeable
newtype Income = Income String       deriving (Show, Eq, Ord, Typeable)
newtype Student = Student Bool       deriving (Show, Eq, Ord, Typeable)
data CreditRating = Fair | Excellent deriving (Show, Eq, Ord, Typeable)
data BuysComputer = Yes  | No        deriving (Show, Eq, Ord, Typeable)


instance Show Age where
    show (Age n) = "Age " ++ show n
    show (AgeRange mbFrom mbTo) = "Age [" ++ strFrom ++ ".." ++ strTo ++ "]"
        where strFrom = maybe "" show mbFrom
              strTo   = maybe "" show mbTo

instance Eq Age where
    (Age x) == (Age y)                   = x == y
    (AgeRange from to) == (Age x)        = (&&) <$> mb (<=) from <*> mb (>=) to $ x
                                           where mb = maybe (const True)
    a@(Age x) == r@(AgeRange from to)    = r == a
    (AgeRange xf xt) == (AgeRange yf yt) = xf == yf && xt == yt

instance Ord Age where
    (Age x)              `compare` (Age y)   = x `compare` y
    r@(AgeRange from to) `compare` a@(Age x) | a == r = EQ
                                             | maybe (const False) (<) to x = LT
                                             | otherwise                    = GT
    a@(Age _) `compare` r@(AgeRange _ _)     = case r `compare` a of LT -> GT
                                                                     GT -> LT
                                                                     EQ -> EQ
    (AgeRange xf xt) `compare` (AgeRange yf yt) | xf == yf && xt == yt = EQ
                                                | maybe True (maybe (const False) (<) xt) yt = LT
                                                | maybe True (maybe (const False) (>) xf) yf = GT

data Entry = Entry{ age         :: Age
                  , income      :: Income
                  , student     :: Student
                  , credRating  :: CreditRating
                  , buysComp    :: BuysComputer
                  }
              deriving (Show, Eq)

instance L.Attribute Age where
    possibleDiscreteDomains _ = [
       [ [AgeRange Nothing (Just 30)], [AgeRange (Just 31) (Just 40)],[ AgeRange (Just 40) Nothing] ] -- TODO not the best implementation
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
        case name of "age"           -> L.Attr . age
                     "income"        -> L.Attr . income
                     "student"       -> L.Attr . student
                     "credit rating" -> L.Attr . credRating

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
treeSeniorFair      = Decision (Map.fromList [(False, 2)]) (Just "Fair")
treeSeniorExcellent = Decision (Map.fromList [(True, 3)])  (Just "Excellent")

