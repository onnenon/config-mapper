module Sandbox where

type Ingredient = String

type Amount = Int

type Recipe = [(Ingredient, Amount)]

type Storage = [(Ingredient, Amount)]

combineOnKey :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
combineOnKey as bs = [(k, y1 `div` y2) | (k, y1) <- as, (k', y2) <- bs, k == k']

--- >>> combineOnKey [("a", 3), ("b", 4)] [("a", 1), ("b", 2)]
-- [("a",3),("b",2)]

minValue :: (Eq a) => [(a, Int)] -> Int
minValue xs = minimum [y | (_, y) <- xs]

--- >>> minValue [("a", 3), ("b", 4)]
-- 3

cakes :: Recipe -> Storage -> Int
cakes recipe storage = if length combined /= length recipe then 0 else minimum [v | (_, v) <- combined]
  where
    combined = [(k, v1 `div` v2) | (k, v1) <- storage, (k2, v2) <- recipe, k == k2]

cakes2 :: Recipe -> Storage -> Int
cakes2 recipe storage = minimum $ map amountForIngredient recipe
  where
    amountForIngredient (w, q) = maybe 0 (`div` q) $ lookup w storage
