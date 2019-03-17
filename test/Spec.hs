module Main where

import qualified BellmanFord.Spec                   as BellmanFord
import qualified Queue.Spec                         as Queue
import qualified Test.Hspec.Runner                  as Runner


scDepth :: Int
scDepth = 3

main :: IO ()
main = do
    Runner.hspecWith cfg Queue.spec
    Runner.hspecWith cfg BellmanFord.spec
  where
    cfg = Runner.defaultConfig { Runner.configSmallCheckDepth = scDepth }
