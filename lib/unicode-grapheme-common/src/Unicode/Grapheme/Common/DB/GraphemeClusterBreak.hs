{-# LANGUAGE UndecidableInstances #-}

module Unicode.Grapheme.Common.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak (..),
  )
where

import Data.Coerce (coerce)
import Data.Hashable (Hashable)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed (MVector, Unbox, Vector)
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

newtype instance MVector s GraphemeClusterBreak
  = MV_GraphemeClusterBreak (MVector s Int)

newtype instance Vector GraphemeClusterBreak
  = V_GraphemeClusterBreak (Vector Int)

instance VGM.MVector MVector GraphemeClusterBreak where
  basicInitialize (MV_GraphemeClusterBreak v) = VGM.basicInitialize v
  {-# INLINE basicInitialize #-}

  basicLength (MV_GraphemeClusterBreak v) = VGM.basicLength v
  {-# INLINE basicLength #-}

  basicOverlaps (MV_GraphemeClusterBreak v1) (MV_GraphemeClusterBreak v2) = VGM.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew = fmap MV_GraphemeClusterBreak . VGM.basicUnsafeNew
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeRead (MV_GraphemeClusterBreak v) = fmap toEnum . VGM.basicUnsafeRead v
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeSlice i l (MV_GraphemeClusterBreak v) = coerce $ VGM.basicUnsafeSlice i l v
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeWrite (MV_GraphemeClusterBreak v) i x = VGM.basicUnsafeWrite v i (fromEnum x)
  {-# INLINE basicUnsafeWrite #-}

instance VG.Vector Vector GraphemeClusterBreak where
  basicLength (V_GraphemeClusterBreak v) = VG.basicLength v
  {-# INLINE basicLength #-}

  basicUnsafeFreeze (MV_GraphemeClusterBreak v) = coerce $ VG.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeIndexM (V_GraphemeClusterBreak v) = fmap toEnum . VG.basicUnsafeIndexM v
  {-# INLINE basicUnsafeIndexM #-}

  basicUnsafeSlice i l (V_GraphemeClusterBreak v) = coerce $ VG.basicUnsafeSlice i l v
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeThaw (V_GraphemeClusterBreak v) = coerce $ VG.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}

instance Unbox GraphemeClusterBreak
