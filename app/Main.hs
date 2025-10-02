module Main where

import ConfigMapper (buildConfigMap, invertConfigMapLens)
import Data.Aeson (encode, toJSON)
import Data.ByteString.Lazy qualified as BL

main :: IO ()
main = do
  configMap <- buildConfigMap "configs"
  BL.writeFile "output.json" . encode . toJSON $ invertConfigMapLens configMap