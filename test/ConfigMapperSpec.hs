{-# LANGUAGE OverloadedStrings #-}

module ConfigMapperSpec (spec) where

import ConfigMapper
import Data.Map.Strict qualified as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "invertConfigMap" $ do
    it "inverts an empty config map" $ do
      let input = Map.empty :: ConfigMap
      invertConfigMap input `shouldBe` Map.empty

    it "inverts a single file with one key-value pair" $ do
      let input = Map.singleton "file1.json" (Map.singleton "key1" "value1")
      let expected =
            Map.singleton
              "key1"
              (Map.singleton "value1" ["file1.json"])
      invertConfigMap input `shouldBe` expected

    it "inverts a single file with multiple key-value pairs" $ do
      let input =
            Map.singleton
              "file1.json"
              (Map.fromList [("key1", "value1"), ("key2", "value2")])
      let expected =
            Map.fromList
              [ ("key1", Map.singleton "value1" ["file1.json"]),
                ("key2", Map.singleton "value2" ["file1.json"])
              ]
      invertConfigMap input `shouldBe` expected

    it "inverts multiple files with the same key-value pair" $ do
      let input =
            Map.fromList
              [ ("file1.json", Map.singleton "key1" "value1"),
                ("file2.json", Map.singleton "key1" "value1")
              ]
      let result = invertConfigMap input
      let filesForKey1Value1 = Map.lookup "key1" result >>= Map.lookup "value1"
      fmap (elem "file1.json") filesForKey1Value1 `shouldBe` Just True
      fmap (elem "file2.json") filesForKey1Value1 `shouldBe` Just True
      fmap length filesForKey1Value1 `shouldBe` Just 2

    it "inverts multiple files with different values for the same key" $ do
      let input =
            Map.fromList
              [ ("file1.json", Map.singleton "name" "Alice"),
                ("file2.json", Map.singleton "name" "Bob")
              ]
      let expected =
            Map.singleton
              "name"
              (Map.fromList [("Alice", ["file1.json"]), ("Bob", ["file2.json"])])
      invertConfigMap input `shouldBe` expected

    it "handles the example from the documentation" $ do
      let input =
            Map.fromList
              [ ( "C1.json",
                  Map.fromList
                    [ ("name", "Crescent"),
                      ("title", "America"),
                      ("a_diff_unique_thing", "The moon")
                    ]
                ),
                ( "C2.json",
                  Map.fromList
                    [ ("name", "Crescent"),
                      ("title", "Japanese"),
                      ("unique_thing", "The moon")
                    ]
                )
              ]
      let result = invertConfigMap input
      -- Check that "name" -> "Crescent" includes both files
      let crescentFiles = Map.lookup "name" result >>= Map.lookup "Crescent"
      fmap (elem "C1.json") crescentFiles `shouldBe` Just True
      fmap (elem "C2.json") crescentFiles `shouldBe` Just True
      fmap length crescentFiles `shouldBe` Just 2
      -- Check that "title" has two different values
      let titleMap = Map.lookup "title" result
      fmap (Map.member "Japanese") titleMap `shouldBe` Just True
      fmap (Map.member "America") titleMap `shouldBe` Just True

  describe "invertConfigMapLens" $ do
    it "inverts an empty config map" $ do
      let input = Map.empty :: ConfigMap
      invertConfigMapLens input `shouldBe` Map.empty

    it "inverts a single file with one key-value pair" $ do
      let input = Map.singleton "file1.json" (Map.singleton "key1" "value1")
      let expected =
            Map.singleton
              "key1"
              (Map.singleton "value1" ["file1.json"])
      invertConfigMapLens input `shouldBe` expected

    it "inverts a single file with multiple key-value pairs" $ do
      let input =
            Map.singleton
              "file1.json"
              (Map.fromList [("key1", "value1"), ("key2", "value2")])
      let expected =
            Map.fromList
              [ ("key1", Map.singleton "value1" ["file1.json"]),
                ("key2", Map.singleton "value2" ["file1.json"])
              ]
      invertConfigMapLens input `shouldBe` expected

    it "produces the same result as invertConfigMap for complex input" $ do
      let input =
            Map.fromList
              [ ( "C1.json",
                  Map.fromList
                    [ ("name", "Crescent"),
                      ("title", "America"),
                      ("a_diff_unique_thing", "The moon")
                    ]
                ),
                ( "C2.json",
                  Map.fromList
                    [ ("name", "Crescent"),
                      ("title", "Japanese"),
                      ("unique_thing", "The moon")
                    ]
                )
              ]
      invertConfigMapLens input `shouldBe` invertConfigMap input
