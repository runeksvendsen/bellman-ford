module Main where

import qualified Data.Graph.BellmanFord.Unboxed     as Lib
import qualified BellmanFord.Spec                   as BellmanFord
import qualified Dijkstra.Spec                      as Dijkstra
import qualified Digraph.Spec                       as Digraph
import qualified Queue.Spec                         as Queue
import qualified IndexMinPQ.Spec                    as IndexMinPQ
import qualified Util.Spec                          as Util
import qualified Types.CLI.PrintTrace as CLI

import qualified Test.Tasty                         as Tasty
import           Test.Tasty.SmallCheck              as SC
import qualified Test.Tasty.QuickCheck              as QC
import qualified Test.Tasty.Options as Tasty
import Data.Proxy (Proxy(Proxy))

main :: IO ()
main = runTestTree $
    Tasty.askOption $ \(CLI.PrintTrace printTrace) ->
        let runBF :: BellmanFord.RunBF weight s v meta a
            runBF =
                if printTrace
                    then Lib.runBFTrace
                    else Lib.runBF
        in Tasty.testGroup "Properties" $
            [ mkLocalOption 5 $ Util.spec
            , mkLocalOption 5 $ Queue.spec
            , mkLocalOption 5 $ IndexMinPQ.spec printTrace
            , mkLocalOption 3 $ Digraph.spec
            , mkLocalOption 3 $ BellmanFord.spec runBF
            -- , mkLocalOption 3 $ Dijkstra.spec
            ]
  where
    mkLocalOption scDepth =
        Tasty.localOption (SC.SmallCheckDepth scDepth) .
        Tasty.localOption (QC.QuickCheckTests 4000)

    printTraceOption = Tasty.Option (Proxy :: Proxy CLI.PrintTrace)

    runTestTree =
        Tasty.defaultMainWithIngredients $
            Tasty.includingOptions [printTraceOption] : Tasty.defaultIngredients
