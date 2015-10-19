module Main (main) where

import DecisionTrees
import RunC45Weka

import System.Environment
import System.Exit
import Data.Maybe (listToMaybe, fromMaybe)


main :: IO()
main = getArgs >>= parse


maybeRead = fmap fst . listToMaybe . reads

maybeReadInUnit :: String -> Maybe Float
maybeReadInUnit s = (maybeRead s :: Maybe Float) >>= f
            where f x | x <= 1 && x >= 0 = Just x
                      | otherwise        = Nothing


parse ["-h"] = usage >> exitSuccess

parse [fname] = do
    res <- run fname
    putStrLn ""
    drawDecisionTree res


parse [fname, "--iter", tp] = do
    let tperc  = fromMaybe tpError $ maybeReadInUnit tp
    res <- runIterative fname tperc
    putStrLn ""
    drawDecisionTree res


parse _ = unknownCmd >> usage >> exitFailure

unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: C45Weka [-h] file [--iter tp]"
           putStrLn "       where file is an *.arff nominal data file" -- TODO: numerics!
           putStrLn "             --iter tp - run in iterative mode with tp percent forming test set"

tpError = error "--iter argument must  must be a Float in [0, 1]"
