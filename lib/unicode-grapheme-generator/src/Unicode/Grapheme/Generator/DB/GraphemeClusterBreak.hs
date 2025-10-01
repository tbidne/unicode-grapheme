module Unicode.Grapheme.Generator.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak (..),
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- NOTE: Should be kept in sync with
-- Unicode.Grapheme.Internal.DB.GraphemeClusterBreak.
--
-- It would be nice to have a test for this, but unfortunately internal's
-- GraphemeClusterBreak is (intentionally) not exposed by unicode-grapheme
-- (and we cannot depend on unicode-grapheme-internal since it is private).
-- Two potential options:
--
--   - Import using TH: https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
--
--   - Set unicode-grapheme-internal new test-only field, if it is added:
--     https://github.com/haskell/cabal/issues/10900

data GraphemeClusterBreak
  = GraphemeClusterBreak_CR
  | GraphemeClusterBreak_LF
  | GraphemeClusterBreak_Control
  | GraphemeClusterBreak_Extend
  | GraphemeClusterBreak_ZWJ
  | GraphemeClusterBreak_Regional_Indicator
  | GraphemeClusterBreak_Prepend
  | GraphemeClusterBreak_SpacingMark
  | GraphemeClusterBreak_L
  | GraphemeClusterBreak_V
  | GraphemeClusterBreak_T
  | GraphemeClusterBreak_LV
  | GraphemeClusterBreak_LVT
  | GraphemeClusterBreak_Any
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (Hashable)
