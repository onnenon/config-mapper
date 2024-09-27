module ConfigMapperSpec where

import ConfigMapper (invertMap)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "invertMap" $ do
    it "inverts a nested map correctly" $ do
      let input =
            M.fromList
              [ ("s1", M.fromList [("one", "foo"), ("two", "bar")]),
                ("s2", M.fromList [("one", "foo"), ("two", "baz"), ("three", "xyz")])
              ]
      let expectedOutput =
            M.fromList
              [ ("one", M.fromList [("foo", ["s1", "s2"])]),
                ("two", M.fromList [("bar", ["s1"]), ("baz", ["s2"])]),
                ("three", M.fromList [("xyz", ["s2"])])
              ]
      let actualOutput = invertMap input
      sortMap actualOutput `shouldBe` sortMap expectedOutput

sortMap :: (Ord v) => Map k (Map k' [v]) -> Map k (Map k' [v])
sortMap = M.map (M.map sort)