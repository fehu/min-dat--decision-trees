{- |

Module      : DecisionTrees.Utils
Description : Different utilities.
License     : MIT
Stability   : dev
-}
module DecisionTrees.Utils (

  preservingArg
, sortingGroupBy

) where

import Control.Arrow ((&&&))
import Data.List
import Data.Function

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
