module Unit.Unicode.Grapheme.Internal.ClusterState (tests) where

import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Unicode.Grapheme.Internal.ClusterState
  ( ClusterState
      ( MkClusterState,
        clusters,
        input,
        inputIdx,
        lastRule
      ),
    Rule (MkRule),
  )
import Unicode.Grapheme.Internal.ClusterState qualified as ClusterState
import Unicode.Grapheme.Internal.V15_1 qualified as V15_1
import Unicode.Grapheme.Internal.V15_1.DB qualified as V15_1.DB

tests :: TestTree
tests =
  testGroup
    "Unicode.Grapheme.Internal.ClusterState"
    [ testDisplayClusterStates,
      testRuleLazy
    ]

testDisplayClusterStates :: TestTree
testDisplayClusterStates = testCase desc $ do
  let states = ClusterState.breakGraphemeClustersStates db rules txt
      displayed = T.lines $ ClusterState.displayClusterStates states

  length expected @=? length displayed
  for_ (zip expected displayed) $ \(e, r) -> do
    e @=? r
  where
    desc = "Displays ClusterState"

    expected =
      [ "Input: 0061 1F3FF 1F476 200D 1F6D1",
        "Step 0. <no rule>:",
        "Step 1. GB1: 0061",
        "Step 2. GB9: 0061 × 1F3FF",
        "Step 3. GB999: 0061 × 1F3FF ÷ 1F476",
        "Step 4. GB9: 0061 × 1F3FF ÷ 1F476 × 200D",
        "Step 5. GB11: 0061 × 1F3FF ÷ 1F476 × 200D × 1F6D1",
        "Step 6. GB2: 0061 × 1F3FF ÷ 1F476 × 200D × 1F6D1"
      ]

    -- 15.1 GraphemeBreakTest.txt 1195
    txt = "\x0061\x1F3FF\x1F476\x200D\x1F6D1"

testRuleLazy :: TestTree
testRuleLazy = testCase desc $ do
  let result = ClusterState.breakGraphemeClusters db [r1, r2] ""
  [] @=? result
  where
    desc = "Rule Semigroup is lazy"

    r1 = MkRule $ \_ _ -> Just (mkClusterState "R1")
    r2 = error "bad rule"

db :: V15_1.DB.UnicodeDatabase
db = V15_1.DB.database

rules :: [Rule V15_1.DB.UnicodeDatabase]
rules = V15_1.rules

mkClusterState :: Text -> ClusterState
mkClusterState r =
  MkClusterState
    { lastRule = Just r,
      clusters = mempty,
      input = mempty,
      inputIdx = 1
    }
