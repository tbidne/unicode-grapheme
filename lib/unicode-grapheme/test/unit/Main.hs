module Main (main) where

import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)
import Unit.Unicode.Grapheme qualified
import Unit.Unicode.Grapheme.Internal.ClusterState qualified
import Unit.Utils qualified

main :: IO ()
main = do
  -- Params cannot be used via withResource since we require them to be
  -- pure, as they are used in test generation. withResource intentionally
  -- requires IO to prevent this.
  graphemeBreakTestsParams <- Unit.Utils.readGraphemeBreakTestsParams

  defaultMainWithRerun $
    testGroup
      "Unit tests"
      [ Unit.Unicode.Grapheme.tests graphemeBreakTestsParams,
        Unit.Unicode.Grapheme.Internal.ClusterState.tests
      ]
