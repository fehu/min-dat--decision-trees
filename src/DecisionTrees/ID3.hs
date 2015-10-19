{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- |
--
-- Module      :  DecisionTrees.ID3
-- License     :  MIT
-- Stability   :  dev

module DecisionTrees.ID3 (

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

import DecisionTrees.Definitions
import DecisionTrees.TreeBranching
import DecisionTrees.TreeBranching.Debug
import DecisionTrees.Utils


information :: Int -> Int -> Float
information x y = information' [x, y]

entropy :: [(Int, Int)] -> Float
entropy = entropy' . map (\x -> map ($ x) [fst, snd])



information' :: [Int] -> Float
information' ns = sum $ map f ns
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
        selectBestAttrSplitting entries except =
            case selectBestAttrSplitting' entries except of
                []        -> error "empty selectBestAttrSplitting'"
                (bs, _):_ -> bs

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
        selectBestAttrSplitting' [] _ = error "empty entries"
        selectBestAttrSplitting' entries except = sortSplit
            where splitByGain = do (Attr attr) <- listAttributes (head entries)
                                   attrSplit   <- possibleDiscreteDomains attr

                                   let attrSplitVS = map (\as -> ( attributeName attr
                                                                 , Set.fromList $ map Attr as)
                                                         ) attrSplit
                                   let cc = do (attr', entries') <- splitEntries entries attrSplitVS
                                               return $ countClasses entries'
                                   let c = map (map snd) cc

                                   case attrSplitVS of (a,_):_ -> if a `Set.notMember` except
                                                                  then return ((attrSplitVS, gain' c), cc)
                                                                  else []
                                                       []      -> error "empty attrSplitVS"

                  sortSplit = sortBy (compare `on` (negate . snd . fst)) splitByGain


countClasses :: (Entry entry) => [entry] -> [(AttributeContainer, Int)]
countClasses [] = []
countClasses entries = sortBy (compare `on` fst) $ Map.assocs mp
    where entry = head entries
          mp = Map.union (Map.fromList . sortingGroupBy getClass length $ entries)
                         (Map.fromSet (const 0) $ classDomain entry)


