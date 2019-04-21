module Util.Spec
( spec
)
where

import           Types.ListWithItem                 (ListWithItem(..))
import qualified Util
import qualified Data.List.NonEmpty                 as NE
import qualified Util.QuickSmall                    as QS
import qualified Test.Tasty                         as Tasty


spec :: Tasty.TestTree
spec = Tasty.testGroup "Util" $
    [ Tasty.testGroup "sameUniqueSequenceAs" $
        QS.testProperty "true for all instatiations of a sequence" $
            \(ListWithItem lst item) ->
                NE.toList lst `Util.test_sameUniqueSequenceAs` (item :: Int)
    ]
