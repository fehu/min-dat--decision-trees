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
, entropy'
, gain
, gain'

) where

import Control.Arrow
import GHC.Float
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (maximumBy, transpose, sortBy)
import Data.Function (on)

import DecisionTrees.Learning
import DecisionTrees.Learning.Debug
import DecisionTrees.Utils


information :: Int -> Int -> Float
information x y = information' [x, y]

--information p n = f pf + f nf
--    where f x = - uncurry (*) ((id &&& logBase 2 ) x)
--          pf = int2Float p / (int2Float p + int2Float n)
--          nf = int2Float n / (int2Float p + int2Float n)

entropy :: [(Int, Int)] -> Float
entropy = entropy' . map (\x -> map ($ x) [fst, snd])
--entropy subs = sum entr
--    where entr = do (p, n) <- subs
--                    let frac = int2Float (p+n) / totalFl
--                    return $ frac * information p n
--          totalP = sum $ map fst subs
--          totalN = sum $ map snd subs
--          totalFl = int2Float (totalP + totalN)



information' :: [Int] -> Float
information' ns = sum $ map f ns
-- | 0 `elem` ns = 0
--                | otherwise   = sum $ map f ns
    where f 0    = 0
          f x    = - uncurry (*) ((id &&& logBase 2 ) (frac x))
          frac x = int2Float x / nSum
          nSum   = int2Float $ sum ns

entropy' :: [[Int]] -> Float
entropy' ns = sum . map f $ ns
    where f xs = int2Float (sum xs) / nSum * information' xs
          nSum = int2Float . sum . map sum $ ns


gain :: [(Int, Int)] -> Float
gain = gain' . map (\x -> map ($ x) [fst, snd])

gain' :: [[Int]] -> Float
gain' xs = information' (map sum $ transpose xs) - entropy' xs

instance (Entry entry) =>
    TreeBranching entry where
     -- selectBestAttrSplitting :: [entry] -> Set AttributeName -> ([AttrValSet], Float)
        selectBestAttrSplitting entries = fst . head . selectBestAttrSplitting' entries

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

instance (Entry entry) =>
    TreeBranchingDebug entry where
        selectBestAttrSplitting' entries except = sortSplit
            where splitByGain = do (Attr attr) <- listAttributes (head entries)
                                   attrSplit   <- possibleDiscreteDomains attr

                                   let attrSplitVS = map (\as -> ( attributeName attr
                                                                 , Set.fromList $ map Attr as)
                                                         ) attrSplit
                                   let cc = do (attr', entries') <- splitEntries entries attrSplitVS
                                               return $ countClasses entries'
--                                               if fst attr' `Set.notMember` except
--                                                then return $ countClasses attr' entries'
--                                                else []
                                   let c = map (map snd) cc

                                   if (fst . head $ attrSplitVS) `Set.notMember` except
                                    then return ((attrSplitVS, gain' c), cc)
                                    else []
--                  gain xs   = information' (map sum xs) - entropy' xs
--                  sortSplit = sortBy (compare `on` snd) splitByGain
                  sortSplit = sortBy (compare `on` (negate . snd . fst)) splitByGain

--countClasses :: (Entry entry) => [entry] -> Map AttributeContainer Int
--countClasses = Map.fromList . sortingGroupBy getClass length

countClasses :: (Entry entry) => [entry] -> [(AttributeContainer, Int)]
countClasses entries = sortBy (compare `on` fst) $ Map.assocs mp
    where entry = head entries  -- TODO dangerous
          mp = Map.union (Map.fromList . sortingGroupBy getClass length $ entries)
                         (Map.fromSet (const 0) $ classDomain entry)


