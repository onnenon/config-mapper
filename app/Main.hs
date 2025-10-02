module Main where

import Control.Lens (at, ix, (%~), (&), _Just)
import Data.Aeson (decode, encode, toJSON)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath ((</>))

type ConfigMap = Map FilePath (Map Text Text)

type InvertedConfigMap = Map Text (Map Text [FilePath])

main :: IO ()
main = do
  configMap <- buildConfigMap "configs"
  BL.writeFile "output.json" . encode . toJSON $ invertConfigMapLens configMap

buildConfigMap :: FilePath -> IO ConfigMap
buildConfigMap dir = do
  files <- listDirectory dir
  Map.fromList . catMaybes <$> traverse processFile files
  where
    processFile f = do
      result <- decodeFile (dir </> f)
      pure $ (,) f <$> result

decodeFile :: FilePath -> IO (Maybe (Map Text Text))
decodeFile = fmap decode . BL.readFile

-- Original implementation for comparison
invertConfigMap :: ConfigMap -> InvertedConfigMap
invertConfigMap = Map.foldrWithKey' addFileToInvertedMap Map.empty
  where
    addFileToInvertedMap fileName innerMap acc =
      Map.foldrWithKey' (addKeyValueToInvertedMap fileName) acc innerMap

    addKeyValueToInvertedMap fileName key value =
      Map.insertWith (Map.unionWith (++)) key (Map.singleton value [fileName])

-- Lens-based implementation
invertConfigMapLens :: ConfigMap -> InvertedConfigMap
invertConfigMapLens = Map.foldrWithKey' processFile Map.empty
  where
    processFile :: FilePath -> Map Text Text -> InvertedConfigMap -> InvertedConfigMap
    processFile fileName configItems invertedMap =
      Map.foldrWithKey' (processKeyValue fileName) invertedMap configItems

    processKeyValue :: FilePath -> Text -> Text -> InvertedConfigMap -> InvertedConfigMap
    processKeyValue fileName key value invertedMap =
      invertedMap
        -- Create/access the outer map entry for the key, ensuring it exists
        & at key %~ Just . fromMaybe Map.empty
        -- Create/update the inner map entry for the value, ensuring it exists
        & at key . _Just . at value %~ Just . fromMaybe []
        -- Append the fileName to the list
        & at key . _Just . ix value %~ (fileName :)