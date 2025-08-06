module Main (main) where

import Data.Foldable (for_)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Unicode.Grapheme qualified as Grapheme
import Unicode.Grapheme.Generator.Version qualified as Gen.Version

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Generator shared types"
      [ testVersionsSync
      ]

-- It would be nice to test GraphemeClusterBreak too, but unfortunately that
-- is (intentionally) not exposed by unicode-grapheme (and we cannot depend
-- on unicode-grapheme-internal since it is private). Two potential options:
--
--   - Import using TH: https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
--
--   - Set unicode-grapheme-internal new test-only field, if it is added:
--     https://github.com/haskell/cabal/issues/10900

testVersionsSync :: TestTree
testVersionsSync = testCase desc $ do
  length as @=? length bs
  for_ (zip as bs) $ \(a, b) -> do
    show a @=? show b
    Gen.Version.displayVersion @String a @=? Grapheme.displayVersion b
  where
    desc = "UnicodeVersions are in sync"

    as :: [Gen.Version.UnicodeVersion]
    as = [minBound .. maxBound]

    bs :: [Grapheme.UnicodeVersion]
    bs = [minBound .. maxBound]
