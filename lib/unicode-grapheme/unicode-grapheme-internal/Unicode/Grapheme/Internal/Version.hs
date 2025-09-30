{-# LANGUAGE CPP #-}

module Unicode.Grapheme.Internal.Version
  ( -- * Version
    UnicodeVersion (..),

    -- * Display
    displayVersion,
  )
where

import Data.String (IsString)

-- | Unicode versions supported by this library. The following table lists the
-- unicode versions for @base@. The 'Supported' column refers to if the
-- unicode version itself is supported here i.e. X.Y is "supported" if
-- UnicodeVersion_X_Y exists.
--
-- +------+---------+-----------+
-- | Base | Unicode | Supported |
-- +======+=========+===========+
-- | 4.21 |    16.0 |         🌕 |
-- +------+---------+-----------+
-- | 4.20 |    15.1 |         🌕 |
-- +------+---------+-----------+
-- | 4.19 |    15.1 |         🌕 |
-- +------+---------+-----------+
-- | 4.18 |    15.0 |         🌕 |
-- +------+---------+-----------+
-- | 4.17 |    14.0 |         🌕 |
-- +------+---------+-----------+
-- | 4.16 |    14.0 |         🌕 |
-- +------+---------+-----------+
--
-- @since 0.1
data UnicodeVersion
  = -- | @since 0.1
    UnicodeVersion_14_0
  | -- | @since 0.1
    UnicodeVersion_15_0
  | -- | @since 0.1
    UnicodeVersion_15_1
  | -- | @since 0.1
    UnicodeVersion_16_0
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | Textual representation.
--
-- @since 0.1
displayVersion :: (IsString s) => UnicodeVersion -> s
displayVersion UnicodeVersion_14_0 = "14.0"
displayVersion UnicodeVersion_15_0 = "15.0"
displayVersion UnicodeVersion_15_1 = "15.1"
displayVersion UnicodeVersion_16_0 = "16.0"
