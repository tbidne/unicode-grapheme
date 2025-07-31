{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- - -XUndecidableInstances needed for UnsupportedF and GHC 9.4, apparently.
-- - -Wno-redundant-constraints for TypeError.

-- | Text grapheme utilities.
--
-- @since 0.1
module Unicode.Grapheme
  ( -- * Primary
    breakGraphemeClusters,

    -- * Base
    breakGraphemeClustersBase,

    -- * Unicode versions
    UnicodeVersion (..),
    breakGraphemeClustersVersion,

    -- ** Functions
    Version.getBaseUnicodeVersion,
    Version.getBaseUnicodeVersionIO,
    Version.getBaseUnicodeVersionOrLatest,

    -- ** Display
    Version.displayVersion,

    -- ** Errors
    Version.UnsupportedUnicodeE (..),
  )
where

import Data.Text (Text)
#if MIN_VERSION_base(4, 18, 0)
import Unicode.Grapheme.Internal.Utils qualified as Utils
#else
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage ((:<>:)))
import GHC.TypeLits qualified as TypeLits
#endif
import Unicode.Grapheme.Common.Version
  ( UnicodeVersion
      ( UnicodeVersion_15_1,
        UnicodeVersion_16_0
      ),
  )
import Unicode.Grapheme.Common.Version qualified as Version
import Unicode.Grapheme.Internal.V15_0 qualified as V15_0
import Unicode.Grapheme.Internal.V15_1 qualified as V15_1
import Unicode.Grapheme.Internal.V16_0 qualified as V16_0

-- | Breaks 'Text' into grapheme clusters. Uses base's unicode version if
-- it is supported. Otherwise, usage is a type error.
--
-- @since 0.1
#if MIN_VERSION_base(4, 18, 0)

breakGraphemeClustersBase :: Text -> [Text]
breakGraphemeClustersBase =
  breakGraphemeClustersVersion
    $$(Utils.liftIOToTH Version.getBaseUnicodeVersionIO)

-- NOTE:
--
-- It would be really nice if we could use Unsatisfiable here, since
-- we would not have to muck around with this type family (needed because
-- TypeError is unnecessarily strict, here).
--
-- Alas, Unsatisfiable was only added to base in 4.19 (GHC 9.8), which have
-- fully supported unicodes. Thus the only scenario where it could be useful,
-- is for some _later_ version that is unsupported i.e. base comes out with
-- a new unicode version that we cannot immediately support, for some reason.
--
-- Hence for now we leave this commented code as a note for future reference,
-- and use TypeError.
--
--     breakGraphemeClusters :: (TypeError.Unsatisfiable Msg) => Text -> [Text]
--     breakGraphemeClusters = TypeError.unsatisfiable

#else

type UnsupportedMsg :: ErrorMessage
type UnsupportedMsg =
  TypeLits.Text "Unicode < 15.0 (base < 4.18) is not supported. "
    :<>: TypeLits.Text "Please use breakGraphemeClustersVersion "
    :<>: TypeLits.Text "with explicit unicode version."

type UnsupportedF :: Constraint
type family UnsupportedF where
  UnsupportedF = TypeLits.TypeError UnsupportedMsg

breakGraphemeClusters :: UnsupportedF => Text -> [Text]
breakGraphemeClusters = error "unreachable"

#endif

-- | Breaks 'Text' into grapheme clusters. Uses base's unicode version if
-- it is supported. Otherwise falls back to the latest supported version.
--
-- @since 0.1
breakGraphemeClusters :: Text -> [Text]
breakGraphemeClusters =
  breakGraphemeClustersVersion Version.getBaseUnicodeVersionOrLatest

-- | Breaks 'Text' into grapheme clusters for the specified unicode version.
--
-- @since 0.1
breakGraphemeClustersVersion ::
  -- | Unicode version.
  UnicodeVersion ->
  -- | Text to split.
  Text ->
  -- | Grapheme clusters.
  [Text]
breakGraphemeClustersVersion vers = breakFn
  where
    breakFn = case vers of
      Version.UnicodeVersion_15_0 -> V15_0.breakGraphemeClusters
      Version.UnicodeVersion_15_1 -> V15_1.breakGraphemeClusters
      Version.UnicodeVersion_16_0 -> V16_0.breakGraphemeClusters
