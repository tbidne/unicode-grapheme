{-# OPTIONS_GHC -Wno-unused-imports #-}

-- NOTE: For GHC < 9.6, -Wunused-packages is tripped by the dependency on
-- unicode-grapheme-internal (confusingly, the error message mentions
-- unicode-grapheme). To avoid this, we unconditionally import
-- unicode-grapheme-internal (even though it is not used in older GHCs),
-- and disable -Wunused-imports.

module Unit.Unicode.Grapheme (tests) where

import Control.Monad (unless)
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Unicode.Grapheme qualified as Grapheme
import Unicode.Grapheme qualified as Version
import Unicode.Grapheme.Common.Version
  ( UnicodeVersion
      ( UnicodeVersion_14_0,
        UnicodeVersion_15_0,
        UnicodeVersion_15_1,
        UnicodeVersion_16_0
      ),
  )
import Unicode.Grapheme.Common.Version qualified as Version
import Unicode.Grapheme.Internal.ClusterState
  ( ClusterState,
    RulesMatched (unRulesMatched),
    displayClusterStates,
  )
import Unicode.Grapheme.Internal.V14_0 qualified as V14_0
import Unicode.Grapheme.Internal.V15_0 qualified as V15_0
import Unicode.Grapheme.Internal.V15_1 qualified as V15_1
import Unicode.Grapheme.Internal.V16_0 qualified as V16_0
import Unit.Utils (GraphemeBreakTestLine (rules), GraphemeBreakTestsParams)
import Unit.Utils qualified

tests :: GraphemeBreakTestsParams -> TestTree
tests params =
  testGroup
    "Unicode.Grapheme"
    [ breakGraphemeClusterTests params
    ]

breakGraphemeClusterTests :: GraphemeBreakTestsParams -> TestTree
breakGraphemeClusterTests params =
  testGroup
    "breakGraphemeCluster"
    [ mkGraphemeBreakTests params,
      exampleBreakTests,
      widthTests
    ]

mkGraphemeBreakTests :: GraphemeBreakTestsParams -> TestTree
mkGraphemeBreakTests params =
  testGroup
    "GraphemeBreakTest.txt"
    (versionGraphemeBreakTests params)

versionGraphemeBreakTests :: GraphemeBreakTestsParams -> [TestTree]
versionGraphemeBreakTests params =
  allUnicodeVersions <&> \v ->
    let ts = Unit.Utils.versionToParams v params
     in testGroup
          (Version.displayVersion v)
          (mkGraphemeBreakTestVersion v <$> ts)

mkGraphemeBreakTestVersion :: UnicodeVersion -> GraphemeBreakTestLine -> TestTree
mkGraphemeBreakTestVersion vers line = testCase desc $ do
  let actual1 = breakFn txt
  expected @=? actual1

  let (rulesMatched, actual2) = breakGraphemeClustersRules vers txt
      actualRules = rulesMatchesToList rulesMatched

  compareStates actual1 actual2
  compareStates line.rules actualRules
  where
    breakFn =
      Grapheme.runUnicodeFunctionVersion vers Grapheme.breakGraphemeClusters

    txt = Unit.Utils.lineToText line
    expected = Unit.Utils.lineToExpected line

    desc = Unit.Utils.displayGraphemeBreakTestLine line

    compareStates :: (Eq a, Show a) => a -> a -> IO ()
    compareStates x y =
      if x == y
        then pure ()
        else do
          let states = breakGraphemeClustersStates vers txt
              msg =
                mconcat
                  [ show x,
                    " /= ",
                    show y,
                    "\n",
                    T.unpack $ displayClusterStates states
                  ]
          assertFailure msg

    rulesMatchesToList :: RulesMatched -> [Text]
    rulesMatchesToList = F.toList . (.unRulesMatched)

breakGraphemeClustersRules :: UnicodeVersion -> Text -> (RulesMatched, [Text])
breakGraphemeClustersRules UnicodeVersion_14_0 = V14_0.breakGraphemeClustersRules
breakGraphemeClustersRules UnicodeVersion_15_0 = V15_0.breakGraphemeClustersRules
breakGraphemeClustersRules UnicodeVersion_15_1 = V15_1.breakGraphemeClustersRules
breakGraphemeClustersRules UnicodeVersion_16_0 = V16_0.breakGraphemeClustersRules

breakGraphemeClustersStates :: UnicodeVersion -> Text -> Seq ClusterState
breakGraphemeClustersStates UnicodeVersion_14_0 = V14_0.breakGraphemeClustersStates
breakGraphemeClustersStates UnicodeVersion_15_0 = V15_0.breakGraphemeClustersStates
breakGraphemeClustersStates UnicodeVersion_15_1 = V15_1.breakGraphemeClustersStates
breakGraphemeClustersStates UnicodeVersion_16_0 = V16_0.breakGraphemeClustersStates

exampleBreakTests :: TestTree
exampleBreakTests =
  testGroup
    "Cases"
    [ testExample ["\x1F1EF\x1F1F5"] "\x1F1EF\x1F1F5", -- 🇯🇵
      testExample ["\x1F1EF\x1F1F5", "\x1F1EF\x1F1F5"] "\x1F1EF\x1F1F5\x1F1EF\x1F1F5", -- 🇯🇵🇯🇵
      testExample ["\x4F\x308"] "\x4F\x308", -- Ö
      testExample ["\x1F9D1\x200D\x1F33E"] "\x1F9D1\x200D\x1F33E", -- 🧑‍🌾
      testExample ["ɑ", "Ö", "ɣ", "Ä"] "ɑÖɣÄ"
    ]

testExample :: [Text] -> Text -> TestTree
testExample expected txt = testCase desc $ do
  F.for_ allUnicodeVersions $ \v -> do
    -- Using breakGraphemeClustersVersion so that we do not have to do more
    -- cpp like above.
    let actual = breakFn v txt
    -- Manual equals so we get a better error message.
    unless (expected == actual) $ do
      let msg =
            mconcat
              [ Version.displayVersion v,
                ":\n",
                "  expected: ",
                show expected,
                "\n   but got: ",
                show actual
              ]
      assertFailure msg
  where
    desc = T.unpack txt

    breakFn v =
      Grapheme.runUnicodeFunctionVersion v Grapheme.breakGraphemeClusters

widthTests :: TestTree
widthTests =
  testGroup
    "Width"
    [ testClusterWidth,
      testTextWidth
    ]

testClusterWidth :: TestTree
testClusterWidth = testCase desc $ do
  1 @=? clusterWidth "a"
  1 @=? clusterWidth "\x4F\x308"
  2 @=? clusterWidth "🇯🇵"
  2 @=? clusterWidth "\x1F9D1\x200D\x1F33E"

  -- Size is capped at 2.
  1 @=? clusterWidth "aaa"
  2 @=? clusterWidth "🇯🇵🇯🇵"
  where
    desc = "clusterWidth cases"

    clusterWidth = Grapheme.runUnicodeFunction Grapheme.clusterWidth

testTextWidth :: TestTree
testTextWidth = testCase desc $ do
  1 @=? textWidth "a"
  1 @=? textWidth "\x4F\x308"
  2 @=? textWidth "🇯🇵"
  2 @=? textWidth "\x1F9D1\x200D\x1F33E"

  3 @=? textWidth "aaa"
  4 @=? textWidth "🇯🇵🇯🇵"
  where
    desc = "textWidth cases"

    textWidth = Grapheme.runUnicodeFunction Grapheme.textWidth

allUnicodeVersions :: [UnicodeVersion]
allUnicodeVersions = [minBound .. maxBound]
