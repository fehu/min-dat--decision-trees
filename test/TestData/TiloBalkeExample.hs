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

) where

import DecisionTrees

import qualified Data.Map as Map

data CreditRating = Fair
                  | Excellent
              deriving (Show, Eq)

data Entry = Entry{ age         :: Int
                  , income      :: String
                  , student     :: Bool
                  , credRating  :: CreditRating
                  , buysComp    :: Bool
                  }
              deriving (Show, Eq)

testData :: [Entry]
testData = [
   Entry 25 "high"   False Fair      False
 , Entry 27 "high"   False Excellent False
 , Entry 35 "high"   False Fair      True
 , Entry 42 "medium" False Excellent True
 , Entry 52 "low"    True  Excellent True
 , Entry 45 "low"    True  Fair      False
 , Entry 31 "low"    True  Excellent True
 , Entry 29 "medium" False Fair      False
 , Entry 22 "low"    True  Fair      True
 , Entry 42 "medium" True  Excellent True
 , Entry 30 "medium" True  Excellent True
 , Entry 36 "medium" False Excellent True
 , Entry 33 "high"   True  Fair      True
 , Entry 50 "medium" False Fair      False
 ]

splitAge age | age <= 30 = "<=30"
             | age >  40 = ">40"
             | otherwise = "31..40"



expectedDTree :: Decision Entry String
expectedDTree =
    DecisionStep { prepare = splitAge . age
                 , describePrepare = "age"
                 , select = Map.fromList [ (["<=30"],   treeAgeYoung)
                                         , (["31..40"], treeAgeMiddle)
                                         , ([">40"],    treeAgeSenior)
                                         ]
                 }


treeAgeYoung  = undefined
treeAgeMiddle = undefined
treeAgeSenior = undefined

