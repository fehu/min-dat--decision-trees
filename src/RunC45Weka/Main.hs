module Main (main) where

import DecisionTrees
import RunC45Weka

import System.Environment
import System.Exit


main :: IO()
main = getArgs >>= parse


parse ["-h"] = usage >> exitSuccess
parse [fname] = do
    res <- run fname
    putStrLn ""
    drawDecisionTree res

parse _ = unknownCmd >> usage >> exitFailure

unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: C45Weka [-h] file"
           putStrLn "       where file is an *.arff nominal data file" -- TODO: numerics!
