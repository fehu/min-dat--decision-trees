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

parse [fname, clazz] = do
    res <- run fname clazz
    putStrLn ""
    drawDecisionTree res


parse [fname, clazz, "--iter", tp] = do
    let tperc  = fromMaybe tpError $ maybeReadInUnit tp
    res <- runIterative fname clazz tperc
    putStrLn ""
    drawDecisionTree res


parse _ = unknownCmd >> usage >> exitFailure

unknownCmd = putStrLn "unknown command"

usage = do putStrLn "Usage: ID3Weka [-h] file class [--iter p]\n"
           putStrLn "       file   | *.arff nominal data file" -- TODO: numerics!
           putStrLn "       class  | name of the class attribute"
           putStrLn "     --iter p | run in iterative mode with 'p' percent forming test set"

tpError = error "--iter argument must  must be a Float in [0, 1]"
