{-# LANGUAGE OverloadedStrings #-}

module ConfigMapper
  ( -- * Types
    ConfigMap,
    InvertedConfigMap,

    -- * Core Functions
    buildConfigMap,
    invertConfigMap,
    invertConfigMapLens,

    -- * Helper Functions
    decodeFile,
  )
where

import Control.Lens (at, (%~), (&), _Just)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath ((</>))

-- | Maps file paths to their configuration key-value pairs
type ConfigMap = Map FilePath (Map Text Text)

-- | Inverted index: maps keys to values to the list of files containing that key-value pair
type InvertedConfigMap = Map Text (Map Text [FilePath])

-- | Build a ConfigMap from all JSON files in a directory
buildConfigMap :: FilePath -> IO ConfigMap
buildConfigMap dir = do
  files <- listDirectory dir
  Map.fromList . catMaybes <$> traverse processFile files
  where
    processFile f = do
      result <- decodeFile (dir </> f)
      pure $ (,) f <$> result

-- | Decode a JSON file into a Map of Text key-value pairs
decodeFile :: FilePath -> IO (Maybe (Map Text Text))
decodeFile = fmap decode . BL.readFile

-- | Invert a ConfigMap using fold-based approach
--
-- Transforms a map of files to configs into a map of keys to values to files.
invertConfigMap :: ConfigMap -> InvertedConfigMap
invertConfigMap = Map.foldrWithKey' addFileToInvertedMap Map.empty
  where
    addFileToInvertedMap fileName innerMap acc =
      Map.foldrWithKey' (addKeyValueToInvertedMap fileName) acc innerMap

    addKeyValueToInvertedMap fileName key value =
      Map.insertWith (Map.unionWith (++)) key (Map.singleton value [fileName])

-- | Invert a ConfigMap using lens-based approach
--
-- Transforms a map of files to configs into a map of keys to values to files.
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
        & at key . _Just . at value %~ Just . maybe [fileName] (fileName :)
