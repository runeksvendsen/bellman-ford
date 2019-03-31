module Main where

import qualified BellmanFord.Spec                   as BellmanFord
import qualified Digraph.Spec                       as Digraph
import qualified Queue.Spec                         as Queue

import qualified Test.Tasty                         as Tasty
import           Test.Tasty.SmallCheck              as SC
import qualified Test.Tasty.QuickCheck              as QC


main :: IO ()
main =
    Tasty.defaultMain $
        Tasty.testGroup "Properties" $
            [ mkLocalOption 5 $ Queue.spec
            , mkLocalOption 4 $ Digraph.spec
            , mkLocalOption 4 $ BellmanFord.spec
            ]
  where
    mkLocalOption scDepth =
        Tasty.localOption (SC.SmallCheckDepth scDepth) .
        Tasty.localOption (QC.QuickCheckTests 200)
