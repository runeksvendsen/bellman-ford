{-# LANGUAGE OverloadedStrings #-}

module Digraph.DotGraph
( spec
)
where
import Types.Edge
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import qualified Control.Monad.ST as ST
import qualified Data.Graph.Digraph as DG
import qualified Data.Set as Set
import Test.Hspec.Expectations.Pretty (shouldBe, Expectation)
import qualified Test.Tasty as Tasty
import Control.Monad (forM_)
import qualified Test.Hspec as Hspec
import qualified Test.Tasty.Hspec as Tasty
import System.IO.Unsafe (unsafePerformIO)
import qualified DotParse
import qualified FlatParse.Basic as DotParse
import qualified Data.ByteString as BS
import qualified Data.Text as T

spec :: Tasty.TestTree
spec = unsafePerformIO $
    Tasty.testSpec "graphToDot" $
        Hspec.describe "actual == expected" $
            forM_ allTests $ \test@((_, graphName), _) ->
                Hspec.it graphName (assertTest test)
{-# NOINLINE spec #-}

parseDotGraph
    :: BS.ByteString
    -> Either T.Text DotParse.Graph
parseDotGraph bs =
    case DotParse.runParser DotParse.dotParse bs of
        DotParse.OK res leftovers ->
            if BS.null leftovers
                then Right res
                else Left $ "Leftover bytes: " <> TE.decodeUtf8Lenient leftovers
        DotParse.Fail ->
            Left "failure"
        DotParse.Err e ->
            Left $ "error: " <> T.pack (show e)

assertTest
    :: (([TestEdge String], String), LT.Text)
    -> Expectation
assertTest ((edges, graphName), expected) = do
    let actual = ST.runST $ do
            g <- DG.fromEdges edges
            DG.graphToDot LT.pack (Set.singleton . LT.pack . DG.eMeta) (LT.pack graphName) g
    actual `shouldBe` expected

allTests :: [(([TestEdge String], String), LT.Text)]
allTests =
    [ test1
    ]

test1 :: (([TestEdge String], String), LT.Text)
test1 =
    ((edges, graphName), expected)
    where
        graphName = "test1"

        edges =
            [ "a" --> "b"
            , "b" --> "c"
            ]

        expected =
            let mkEdge :: Int -> Int -> LT.Text -> LT.Text
                mkEdge from to label =
                    LT.pack (show from) <> " -> " <> LT.pack (show to) <> " [label=\"" <> label <> "\"]"
            in LT.unlines $
                 "digraph {" :

                map statement
                    [ "label = \"" <> LT.pack graphName <> "\""
                    , "0 [label=\"a\"]"
                    , mkEdge 0 1 "ab"
                    , "1 [label=\"b\"]"
                    , mkEdge 1 2 "bc"
                    , "2 [label=\"c\"]"
                    ]
                ++ ["}"]


        statement txt = txt <> ";"
        from --> to =
            TestEdge from to (from <> to)
