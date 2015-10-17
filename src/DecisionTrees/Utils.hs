{- |

Module      : DecisionTrees.Utils
Description : Different utilities.
License     : MIT
Stability   : dev
-}
module DecisionTrees.Utils (

  preservingArg
, sortingGroupBy
, fullSubsets

) where

import Control.Arrow ((&&&))
import Data.List
import Data.Function
import qualified Data.Set as Set
import Data.Set (Set)

-- I guess in Haskell should already exist a way to do it,
-- but I don't know it

-- | Preserves an argument and returns it, tupled with function's result
--
-- > preservingArg id x == (x, x)
preservingArg :: (a -> b) -> a -> (a, b)
preservingArg f a = (a, f a)

-- | Groups the elements of a list according to /discriminator function/
--    and applies /result function/ to each formed group
--
-- see <http://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby>
sortingGroupBy :: (Ord b) => (a -> b)   -- ^ /discriminator function/
                          -> ([a] -> c) -- ^ /result function/
                          -> [a]        -- ^ a list
                          -> [(b, c)]   -- result of grouping and /result function/ application
sortingGroupBy f g = map (f . head &&& g)
                         . groupBy ((==) `on` f)
                         . sortBy (compare `on` f)


-- | returns a set of all possible /full sets of subsets/ of the given set.
-- | @x@ is a /full sets of subsets/ of @y@ iff @Set.unions x == y@.
fullSubsets :: (Ord a) => Set a -> Set (Set (Set a))
fullSubsets s
    | Set.size s == 1 = Set.singleton $ Set.singleton s
    | otherwise = Set.unions $ sss:nxt
        where nxt = do e <- Set.toList s
                       let s' = Set.delete e s
                       let ss = fullSubsets s'
                       let se = Set.singleton e
                       return $ Set.map (Set.insert se) ss
              sss = Set.singleton (Set.singleton s)

