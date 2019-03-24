module Main where

import qualified BellmanFord.Spec                   as BellmanFord
import qualified Digraph.Spec                       as Digraph
import qualified Queue.Spec                         as Queue

import qualified Test.Tasty                         as Tasty
import           Test.Tasty.SmallCheck              as SC


main :: IO ()
main =
    Tasty.defaultMain $
        Tasty.testGroup "Properties" $
            [ Tasty.localOption (SC.SmallCheckDepth 5) $ Queue.spec
            , Tasty.localOption (SC.SmallCheckDepth 4) $ Digraph.spec
            , Tasty.localOption (SC.SmallCheckDepth 4) $ BellmanFord.spec
            ]
