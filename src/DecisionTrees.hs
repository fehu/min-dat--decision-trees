-- |
--
-- Module      :  DecisionTrees
-- License     :  MIT
-- Stability   :  dev


module DecisionTrees (

  Decision(..)
--, DecisionTree

) where


import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ( (&&&) )

data Decision entity decision = forall cond . Show cond =>
                                DecisionStep { prepare :: entity -> cond
                                             , select  :: Map cond (Decision entity decision)
                                             , describePrepare :: String
                                             }
                              | Decision { decision :: decision }

instance (Show decision) =>
    Show (Decision entity decision)
        where show (DecisionStep _ sel dp) = dp ++ "\t==>\t" ++ show (Map.keys sel)
              show (Decision d)            = show d

type DecisionTree entity decision = Tree (Decision entity decision)

listChildren :: Decision e d -> [Decision e d]
listChildren (DecisionStep _ sel _) = Map.elems sel
listChildren (Decision _)           = []

decision2Tree = unfoldTree (id &&& listChildren)

--d :: Decision String String
--d = DecisionStep { prepare = length
--                 , select "a" = undefined
--                 }



--data Tree entiry prev this next = Root (entiry -> this) (this -> Bool) [Tree entity this next ?]
