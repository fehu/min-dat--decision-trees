{-# LANGUAGE ImplicitParams #-}

module Main (
 main
) where

import DecisionTrees
import DecisionTrees.Definitions
import DecisionTrees.ID3
import TestData.TiloBalkeExample

import Data.Tree


main = do let ?clazz  = Class classname
          let ?config = FinishedSplittingThreshold 1.0
          x <- buildDecisionTree testData
          let tr = decision2Tree show x
          putStrLn ""
          putStrLn $ drawTree tr
