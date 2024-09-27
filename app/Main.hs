module Main where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as B
import Data.Map (Map)
import Data.Map qualified as M

import ConfigMapper (invertMap)


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