module Util.QuickSmall
( testProperty
, testPropertyQC
)
where

import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.SmallCheck              as SC
import qualified Test.Tasty.QuickCheck              as QC


-- | Test property using both smallcheck and QuickCheck
testProperty
    :: (QC.Testable a, SC.Testable IO a)
    => Tasty.TestName
    -> a
    -> Tasty.TestTree
testProperty name prop =
    Tasty.testGroup name
        [ SC.testProperty "SmallCheck" prop
        , QC.testProperty "QuickCheck" prop
        ]

testPropertyQC
    :: (QC.Testable a)
    => Tasty.TestName
    -> a
    -> Tasty.TestTree
testPropertyQC name prop =
    Tasty.testGroup name
        [ QC.testProperty "QuickCheck" prop
        ]
