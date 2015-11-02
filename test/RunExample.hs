{-# LANGUAGE ImplicitParams #-}

module Main (
 main
) where

import DecisionTrees
import DecisionTrees.Definitions
import DecisionTrees.ID3
import TestData.TiloBalkeExample

import Data.Tree


main = do let ?clazz = Class classname
          x <- buildDecisionTree testData
          let tr = decision2Tree show x
          putStrLn ""
          putStrLn $ drawTree tr
