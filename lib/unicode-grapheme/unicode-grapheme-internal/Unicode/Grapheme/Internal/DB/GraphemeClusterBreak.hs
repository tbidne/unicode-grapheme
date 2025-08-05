module Unicode.Grapheme.Internal.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak (..),
  )
where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | Grapheme cluster break types, based on GraphemeBreakProperty.txt.
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
