module ConfigMapper (invertMap) where

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

-- | The invertMap function takes a ConfigMap and returns an InvertedConfigMap
-- Example:
-- Given the following ConfigMap:
-- @
-- {
--   "serviceOne": {
--     "DB_PASSWORD": "foo"
--     "SHARED_KEY":  "xyz"
--   },
--   "serviceTwo": {
--     "DB_PASSWORD": "bar"
--     "SHARED_KEY":  "xyz"
--   }
-- }
-- @
--
-- The invertMap function will return:
-- @
-- {
--   "DB_PASSWORD": {
--     "foo": ["serviceOne"],
--     "bar": ["serviceTwo"]
--   },
--   "SHARED_KEY": {
--     "xyz": ["serviceOne", "serviceTwo"]
--   }
-- }
-- @
invertMap :: (Ord k1, Ord k2, Ord v) => ConfigMap k1 k2 v -> InvertedConfigMap k2 v k1
invertMap = combineTuples . map convertTuple . toListOfTuples