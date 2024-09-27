module ConfigMapper where

import Data.Map (Map)
import Data.Map qualified as M

-- Type synonyms for better readability
type ConfigMap k1 k2 v = Map k1 (Map k2 v)

type InvertedConfigMap k2 v k1 = Map k2 (Map v [k1])

type Tuple k1 k2 v = (k2, v, k1)

type InvertedConfigMapValue k1 k2 v = (k2, Map v [k1])

-- Convert the nested map into a list of tuples
toListOfTuples :: (Ord k1, Ord k2, Ord v) => ConfigMap k1 k2 v -> [Tuple k1 k2 v]
toListOfTuples xs =
  [ (k2, v, k1)
    | (k1, kvs) <- M.toList xs,
      (k2, v) <- M.toList kvs
  ]

-- Convert each tuple into the desired format
convertTuple :: (Ord k1, Ord k2, Ord v) => Tuple k1 k2 v -> InvertedConfigMapValue k1 k2 v
convertTuple (k2, v, k1) = (k2, M.singleton v [k1])

-- Combine the tuples into the final map
combineTuples :: (Ord k2, Ord v, Ord k1) => [InvertedConfigMapValue k1 k2 v] -> InvertedConfigMap k2 v k1
combineTuples = M.fromListWith (M.unionWith (++))

-- Invert the map
invertMap :: (Ord k1, Ord k2, Ord v) => ConfigMap k1 k2 v -> InvertedConfigMap k2 v k1
invertMap = combineTuples . map convertTuple . toListOfTuples