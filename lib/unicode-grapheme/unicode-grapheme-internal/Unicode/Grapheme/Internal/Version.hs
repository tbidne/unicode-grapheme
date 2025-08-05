{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Internal.Version
  ( -- * Version
    UnicodeVersion (..),

    -- * Base version
    getBaseUnicodeVersionIO,
    getBaseUnicodeVersionOrLatest,
    getBaseUnicodeVersion,

    -- * Display
    displayVersion,

    -- * Errors
    UnsupportedUnicodeE (..),
  )
where

import Control.Exception (Exception (displayException), throwIO)
import Data.Either (fromRight)
import Data.List qualified as L
import Data.String (IsString)
import Data.Version (Version (Version), showVersion)
import GHC.Unicode qualified

-- | Supported unicode versions. The following table lists the unicode
-- versions for each base that is usable with this library. The 'Supported'
-- column refers to if the unicode version itself is supported here.
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

allVersString :: String
allVersString =
  L.intercalate ", " $
    displayVersion <$> [minBound :: UnicodeVersion .. maxBound]

-- | Retrieves base's unicode version or throws 'UnsupportedUnicodeE'.
--
-- @since 0.1
getBaseUnicodeVersionIO :: IO UnicodeVersion
getBaseUnicodeVersionIO = either throwIO pure getBaseUnicodeVersion

-- | Retrieves base's unicode version or the latest, if the former is
-- unsupported.
--
-- @since 0.1
getBaseUnicodeVersionOrLatest :: UnicodeVersion
getBaseUnicodeVersionOrLatest = fromRight maxBound getBaseUnicodeVersion

-- | Retrieves base's unicode version, or an error if it is unsupported.
--
-- @since 0.1
getBaseUnicodeVersion :: Either UnsupportedUnicodeE UnicodeVersion
getBaseUnicodeVersion = case vers of
  [14, 0, 0] -> Right UnicodeVersion_14_0
  [15, 0, 0] -> Right UnicodeVersion_15_0
  [15, 1, 0] -> Right UnicodeVersion_15_1
  [16, 0, 0] -> Right UnicodeVersion_16_0
  _ -> Left $ MkUnsupportedUnicodeE version
  where
    version@(Version vers _) = GHC.Unicode.unicodeVersion

-- | Exception for unsupported unicode version.
--
-- @since 0.1
newtype UnsupportedUnicodeE = MkUnsupportedUnicodeE Version
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception UnsupportedUnicodeE where
  displayException (MkUnsupportedUnicodeE vers) =
    mconcat
      [ "Unsupported unicode version '",
        showVersion vers,
        "'. Supported versions are: ",
        allVersString
      ]
