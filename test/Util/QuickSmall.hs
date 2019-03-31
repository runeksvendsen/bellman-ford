module Util.QuickSmall
( testProperty )
where

import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.SmallCheck              as SC
import qualified Test.Tasty.QuickCheck              as QC


-- | Test property using both smallcheck and QuickCheck
testProperty
    :: (QC.Testable a, SC.Testable IO a)
    => Tasty.TestName
    -> a
    -> [Tasty.TestTree]
testProperty name prop =
    [ SC.testProperty (name ++ " (SmallCheck)") prop
    , QC.testProperty (name ++ " (QuickCheck)") prop
    ]
