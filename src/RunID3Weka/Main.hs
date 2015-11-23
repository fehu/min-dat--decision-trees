module Main (main) where

import DecisionTrees
import RunID3Weka

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

parse [fname, clazz, thr] = do
    let threshold  = fromMaybe thrError $ maybeReadInUnit thr
    res <- run fname clazz (FinishedSplittingThreshold threshold)
    putStrLn ""
    drawDecisionTree res


parse [fname, clazz, thr, "--iter", tp] = do
    let threshold  = fromMaybe thrError $ maybeReadInUnit thr
    let tperc  = fromMaybe tpError $ maybeReadInUnit tp
    res <- runIterative fname clazz (FinishedSplittingThreshold threshold) tperc
    putStrLn ""
    drawDecisionTree res


parse _ = unknownCmd >> usage >> exitFailure

unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: ID3Weka [-h] file class fsth [--iter p]\n"
           putStrLn "       file   | *.arff nominal data file" -- TODO: numerics!
           putStrLn "       class  | name of the class attribute"
           putStrLn "       fsth   | finished splitting threshold"
           putStrLn "     --iter p | run in iterative mode with 'p' percent forming test set"

inUnitError name = error $ name ++ " must  must be a Float in [0, 1]"

tpError = inUnitError "--iter argument"

thrError = inUnitError "finished splitting threshold"
