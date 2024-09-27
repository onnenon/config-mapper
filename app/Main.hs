module Main where

import Control.Monad.State

type Counter = Int

increment :: State Counter Int
increment = do
  counter <- get
  let newCount = counter + 1
  put newCount
  return newCount

main :: IO ()
main = do
  let (result, finalState) = runState increment 0
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Final state: " ++ show finalState
