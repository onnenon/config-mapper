{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as B
import Data.Map (Map)
import Data.Map qualified as M

-- Type synonyms for better readability
type OuterMap k1 k2 v = Map k1 (Map k2 v)

type InnerMap k2 v k1 = Map k2 (Map v [k1])

type Tuple k1 k2 v = (k2, v, k1)

-- Convert the nested map into a list of tuples
toListOfTuples :: (Ord k1, Ord k2, Ord v) => OuterMap k1 k2 v -> [Tuple k1 k2 v]
toListOfTuples xs =
  [ (k2, v, k1)
    | (k1, kvs) <- M.toList xs,
      (k2, v) <- M.toList kvs
  ]

-- Convert each tuple into the desired format
convertTuple :: (Ord k1, Ord k2, Ord v) => Tuple k1 k2 v -> (k2, Map v [k1])
convertTuple (k2, v, k1) = (k2, M.singleton v [k1])

-- Combine the tuples into the final map
combineTuples :: (Ord k2, Ord v, Ord k1) => [(k2, Map v [k1])] -> InnerMap k2 v k1
combineTuples = M.fromListWith (M.unionWith (++))

-- Invert the map
invertMap :: (Ord k1, Ord k2, Ord v) => OuterMap k1 k2 v -> InnerMap k2 v k1
invertMap = combineTuples . map convertTuple . toListOfTuples

main :: IO ()
main = do
  let input :: Map String (Map String String)
      input =
        M.fromList
          [ ("s1", M.fromList [("one", "foo"), ("two", "bar")]),
            ("s2", M.fromList [("one", "foo"), ("two", "baz"), ("three", "xyz")])
          ]
  let invertedMap = invertMap input
  print invertedMap

  let json = encode invertedMap
  B.writeFile "invertedMap.json" json
  putStrLn "Inverted map saved to invertedMap.json"