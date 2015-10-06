{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
--
-- Module      :  DecisionTrees.C45
-- License     :  MIT
-- Stability   :  dev

module DecisionTrees.C45 (

  information
, information'
, entropy

) where

import Control.Arrow
import GHC.Float
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (maximumBy)
import Data.Function (on)

import DecisionTrees.Learning
import DecisionTrees.Utils


information :: Int -> Int -> Float
information p n = f pf + f nf
    where f x = - uncurry (*) ((id &&& logBase 2 ) x)
          pf = int2Float p / (int2Float p + int2Float n)
          nf = int2Float n / (int2Float p + int2Float n)

entropy :: [(Int, Int)] -> Float
entropy subs = sum entr
    where entr = do (p, n) <- subs
                    let frac = int2Float (p+n) / totalFl
                    return $ frac * information p n
          totalP = sum $ map fst subs
          totalN = sum $ map snd subs
          totalFl = int2Float (totalP + totalN)



information' :: [Int] -> Float
information' ns = sum $ map f ns
    where f x    = - uncurry (*) ((id &&& logBase 2 ) (frac x))
          frac x = int2Float x / nSum
          nSum   = int2Float $ sum ns

entropy' :: [[Int]] -> Float
entropy' ns = sum . map f $ ns
    where f xs = int2Float (sum xs) / nSum * information' xs
          nSum = int2Float . sum . map sum $ ns



instance (Entry entry) =>
    TreeBranching entry where
     -- selectBestAttrSplitting :: [entry] -> ([AttributeContainer], Float)
        selectBestAttrSplitting entries = bestSplit
            where splitByGain = do (Attr attr) <- listAttributes (head entries)
                                   attrSplit   <- possibleDiscreteDomains attr

                                   let attrSplit' = map Attr attrSplit
                                   let cc = do (attr', entries') <- splitEntries entries attrSplit'
                                               return $ countClasses entries'
                                   return (attrSplit', gain $ map Map.elems cc)
                  gain xs   = information' (map sum xs) - entropy' xs
                  bestSplit = maximumBy (compare `on` snd) splitByGain

     -- splitEntries :: [entry] -> [AttributeContainer] -> [(AttributeContainer, [entry])]
        splitEntries entries attrs = sortingGroupBy f id entries
            where f entry = undefined


--    finishedSplitting       :: [entry] -> Maybe AttributeContainer

countClasses :: (Entry entry) => [entry] -> Map AttributeContainer Int
countClasses = Map.fromList . sortingGroupBy getClass length


