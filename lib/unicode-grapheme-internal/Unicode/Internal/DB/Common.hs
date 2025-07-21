{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Unicode.Internal.DB.Common
  ( -- * Properties
    PropertiesI (..),
    Properties,

    -- * Creation
    readUnicodeDataIO,
  )
where

import Data.Kind (Type)
import Language.Haskell.TH.Syntax (Lift)
import Unicode.Internal.DB.Common.DerivedCoreProperty
  ( DerivedCorePropertiesI,
  )
import Unicode.Internal.DB.Common.DerivedCoreProperty qualified as DerivedCoreProperty
import Unicode.Internal.DB.Common.EmojiData
  ( EmojiDataI,
  )
import Unicode.Internal.DB.Common.EmojiData qualified as EmojiData
import Unicode.Internal.DB.Common.GraphemeBreakProperty
  ( GraphemeBreakPropertiesF,
    GraphemeBreakPropertiesI,
  )
import Unicode.Internal.DB.Common.GraphemeBreakProperty qualified as GraphemeBreakProperty
import Unicode.Internal.DB.Common.Utils
  ( PropertiesF,
    PropertiesIndex (PropertiesAssertions, PropertiesData),
  )
import Unicode.Internal.Version (UnicodeVersion)

-- | Unicode properties needed for implementing grapheme cluster breaks.
type PropertiesI :: PropertiesIndex -> Type
data PropertiesI p = MkPropertiesI
  { derivedCoreProperties :: DerivedCorePropertiesI p,
    emojiData :: EmojiDataI p,
    graphemeBreakProperties :: GraphemeBreakPropertiesI p
  }

instance
  ( Semigroup (GraphemeBreakPropertiesF p),
    Semigroup (PropertiesF p)
  ) =>
  Semigroup (PropertiesI p)
  where
  MkPropertiesI a1 a2 a3 <> MkPropertiesI b1 b2 b3 =
    MkPropertiesI
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)

instance
  ( Monoid (GraphemeBreakPropertiesF p),
    Monoid (PropertiesF p)
  ) =>
  Monoid (PropertiesI p)
  where
  mempty = MkPropertiesI mempty mempty mempty

-- | Type synonym for DerivedCorePropertiesI data.
type Properties = PropertiesI PropertiesData

-- | Type synonym for DerivedCorePropertiesI assertions.
type PropertyAssertions = PropertiesI PropertiesAssertions

deriving stock instance
  ( Eq (GraphemeBreakPropertiesF p),
    Eq (PropertiesF p)
  ) =>
  Eq (PropertiesI p)

deriving stock instance
  ( Lift (GraphemeBreakPropertiesF p),
    Lift (PropertiesF p)
  ) =>
  Lift (PropertiesI p)

deriving stock instance
  ( Show (GraphemeBreakPropertiesF p),
    Show (PropertiesF p)
  ) =>
  Show (PropertiesI p)

-- | Reads derived core properties corresponding to the given unicode version.
readUnicodeDataIO ::
  PropertyAssertions ->
  UnicodeVersion ->
  IO Properties
readUnicodeDataIO asserts uvers = do
  derivedCoreProperties <-
    DerivedCoreProperty.readUnicodeDataIO
      asserts.derivedCoreProperties
      uvers

  emojiData <-
    EmojiData.readUnicodeDataIO
      asserts.emojiData
      uvers

  graphemeBreakProperties <-
    GraphemeBreakProperty.readUnicodeDataIO
      asserts.graphemeBreakProperties
      uvers

  pure $
    MkPropertiesI
      { derivedCoreProperties,
        emojiData,
        graphemeBreakProperties
      }
