module Main (
 main
) where

import DecisionTrees
import DecisionTrees.C45
import TestData.TiloBalkeExample

import Data.Tree


main = do x <- buildDecisionTree testData
          let tr = decision2Tree show x
          putStrLn ""
          putStrLn $ drawTree tr
