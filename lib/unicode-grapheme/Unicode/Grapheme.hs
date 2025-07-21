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

    -- * Unicode versions
    UnicodeVersion (..),
    breakGraphemeClustersVersion,
    Version.displayVersion,
  )
where

import Data.Text (Text)
#if !MIN_VERSION_base(4, 18, 0)
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage ((:<>:)))
import GHC.TypeLits qualified as TypeLits
#endif
import Unicode.Internal.ClusterState qualified as ClusterState
import Unicode.Internal.V15_0 qualified as V15_0
import Unicode.Internal.V15_1 qualified as V15_1
import Unicode.Internal.V16_0 qualified as V16_0
import Unicode.Internal.Version
  ( UnicodeVersion
      ( UnicodeVersion_15_1,
        UnicodeVersion_16_0
      ),
  )
import Unicode.Internal.Version qualified as Version

-- | 'breakGraphemeClustersVersion' that uses base's unicode version, if it
-- is supported. Otherwise, usage is a type error.
--
-- @since 0.1
#if MIN_VERSION_base(4, 18, 0)

breakGraphemeClusters :: Text -> [Text]
breakGraphemeClusters =
  breakGraphemeClustersVersion $$Version.getBaseVersionTH

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

-- | Breaks 'Text' into grapheme clusters.
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
      Version.UnicodeVersion_15_0 ->
        ClusterState.breakGraphemeClusters
          $$V15_0.databaseTH
          V15_0.rules
      Version.UnicodeVersion_15_1 ->
        ClusterState.breakGraphemeClusters
          $$V15_1.databaseTH
          V15_1.rules
      Version.UnicodeVersion_16_0 ->
        ClusterState.breakGraphemeClusters
          $$V16_0.databaseTH
          V16_0.rules
