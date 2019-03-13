module Main where

import qualified Spec.Spec                          as Spec
import qualified Test.Hspec.Runner                  as Runner


scDepth :: Int
scDepth = 2

main :: IO ()
main = do
    Runner.hspecWith cfg Spec.spec
  where
    cfg = Runner.defaultConfig { Runner.configSmallCheckDepth = scDepth }
