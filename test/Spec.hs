module Main where

import qualified BellmanFord.Spec                   as BellmanFord
import qualified Queue.Spec                         as Queue
import qualified Test.Hspec.Runner                  as Runner
-- Tasty
import qualified Test.Tasty                        as Tasty
import           Test.Tasty.SmallCheck             as SC


scDepth :: Int
scDepth = 4

main :: IO ()
main = do
    Runner.hspecWith cfg Queue.spec
    Runner.hspecWith cfg BellmanFord.spec
    Tasty.defaultMain properties
  where
    cfg = Runner.defaultConfig { Runner.configSmallCheckDepth = scDepth }

properties :: Tasty.TestTree
properties = Tasty.localOption (SC.SmallCheckDepth scDepth) $
   Tasty.testGroup "BellmanFord" BellmanFord.tastyProps
