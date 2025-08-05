module Unicode.Grapheme.Generator.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak (..),
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- NOTE: Should be kept in sync with
-- Unicode.Grapheme.Internal.DB.GraphemeClusterBreak.

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
