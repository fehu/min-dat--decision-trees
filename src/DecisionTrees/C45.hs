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
import qualified Data.Set as Set
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
        selectBestAttrSplitting entries except = bestSplit
            where splitByGain = do (Attr attr) <- listAttributes (head entries)
                                   attrSplit   <- possibleDiscreteDomains attr

                                   let attrSplitVS = map (\as -> ( attributeName attr
                                                                 , Set.fromList $ map Attr as)
                                                         ) attrSplit
                                   let cc = do (attr', entries') <- splitEntries entries attrSplitVS
                                               if fst attr' `Set.notMember` except
                                                then return $ countClasses entries'
                                                else []
                                   return (attrSplitVS, gain $ map Map.elems cc)
                  gain xs   = information' (map sum xs) - entropy' xs
                  bestSplit = maximumBy (compare `on` snd) splitByGain

--                  bestSplit = maximumBy (compare `on` snd) splitByGain

--                  filter ((`Set.member` except) . fst)

     -- splitEntries :: [entry] -> [AttrValSet] -> [(AttrValSet, [entry])]
        splitEntries entries attrValSets =
            do set@(attrName, attrValSet) <- attrValSets
               let entries' = filter (\entry -> attrByName attrName entry `Set.member` attrValSet) entries
               return (set, entries')

     -- finishedSplitting :: [entry] -> Maybe [(AttributeContainer, Int)]
        finishedSplitting entries | null classes        = Just []
                                  | length classes == 1 = Just classes
                                  | otherwise           = Nothing
            where classes = sortingGroupBy getClass length entries
--      TODO
--            where classCount = sortingGroupBy getClass length entries
--                  (classMax, classMaxCoint) = maximumBy (compare `on` snd) classCount

countClasses :: (Entry entry) => [entry] -> Map AttributeContainer Int
countClasses = Map.fromList . sortingGroupBy getClass length


