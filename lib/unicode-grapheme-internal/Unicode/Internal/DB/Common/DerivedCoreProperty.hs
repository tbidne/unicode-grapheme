{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Unicode.Internal.DB.Common.DerivedCoreProperty
  ( -- * Types
    DerivedCorePropertiesI (..),
    DerivedCoreProperties,
    DerivedCorePropertyAssertions,

    -- * Creation
    readUnicodeDataIO,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception (displayException), throwIO)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.HashSet (HashSet)
import Data.HashSet qualified as HSet
import Data.Kind (Type)
import Language.Haskell.TH.Syntax (Lift)
import System.File.OsPath qualified as FileIO
import System.OsPath (osp)
import Unicode.Internal.DB.Common.Utils
  ( PropertiesF,
    PropertiesIndex (PropertiesAssertions, PropertiesData),
  )
import Unicode.Internal.DB.Common.Utils qualified as Common.Utils
import Unicode.Internal.DB.Utils qualified as DB.Utils
import Unicode.Internal.Version (UnicodeVersion)

-- | Properties from DerivedCoreProperties.txt.
type DerivedCorePropertiesI :: PropertiesIndex -> Type
data DerivedCorePropertiesI p = MkDerivedCorePropertiesI
  { -- | Indic_Conjunct_Break=Consonant
    indicConjunctBreakConsonant :: PropertiesF p,
    -- | Indic_Conjunct_Break=Extend
    indicConjunctBreakExtend :: PropertiesF p,
    -- | Indic_Conjunct_Break=Linker
    indicConjunctBreakLinker :: PropertiesF p
  }

instance (Semigroup (PropertiesF p)) => Semigroup (DerivedCorePropertiesI p) where
  MkDerivedCorePropertiesI a1 a2 a3 <> MkDerivedCorePropertiesI b1 b2 b3 =
    MkDerivedCorePropertiesI
      (a1 <> b1)
      (a2 <> b2)
      (a3 <> b3)

instance (Monoid (PropertiesF p)) => Monoid (DerivedCorePropertiesI p) where
  mempty = MkDerivedCorePropertiesI mempty mempty mempty

-- | Type synonym for DerivedCorePropertiesI data.
type DerivedCoreProperties = DerivedCorePropertiesI PropertiesData

-- | Type synonym for DerivedCorePropertiesI assertions.
type DerivedCorePropertyAssertions = DerivedCorePropertiesI PropertiesAssertions

deriving stock instance (Eq (PropertiesF p)) => Eq (DerivedCorePropertiesI p)

deriving stock instance (Lift (PropertiesF p)) => Lift (DerivedCorePropertiesI p)

deriving stock instance (Show (PropertiesF p)) => Show (DerivedCorePropertiesI p)

-- | Derived properties we care about.
data DerivedCoreProperty
  = Indic_Conjunct_Break_Consonant
  | Indic_Conjunct_Break_Extend
  | Indic_Conjunct_Break_Linker
  deriving stock (Eq, Show)

-- | Reads derived core properties corresponding to the given unicode version.
readUnicodeDataIO ::
  DerivedCorePropertyAssertions ->
  UnicodeVersion ->
  IO DerivedCoreProperties
readUnicodeDataIO asserts uvers = do
  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      props = foldMap lineToDerivedProps ls

  checkAsserts props

  pure props
  where
    path =
      Common.Utils.mkUnicodePath uvers [osp|DerivedCoreProperties.txt|]

    checkAsserts :: DerivedCoreProperties -> IO ()
    checkAsserts derived = do
      checkAssert
        Indic_Conjunct_Break_Consonant
        asserts.indicConjunctBreakConsonant
        derived.indicConjunctBreakConsonant

      checkAssert
        Indic_Conjunct_Break_Extend
        asserts.indicConjunctBreakExtend
        derived.indicConjunctBreakExtend

      checkAssert
        Indic_Conjunct_Break_Linker
        asserts.indicConjunctBreakLinker
        derived.indicConjunctBreakLinker

    checkAssert :: DerivedCoreProperty -> Int -> HashSet Char -> IO ()
    checkAssert propType expected derivedSet = do
      let actual = length derivedSet
      unless (expected == actual) $
        throwIO $
          MkDerivedCorePropertiesE
            { propType,
              expected,
              actual
            }

lineToDerivedProps :: ByteString -> DerivedCoreProperties
lineToDerivedProps bs = maybe mempty f (bsToProp bs)
  where
    f (p, c, mC) = case p of
      Indic_Conjunct_Break_Consonant ->
        MkDerivedCorePropertiesI
          { indicConjunctBreakConsonant = HSet.fromList cs,
            indicConjunctBreakExtend = mempty,
            indicConjunctBreakLinker = mempty
          }
      Indic_Conjunct_Break_Extend ->
        MkDerivedCorePropertiesI
          { indicConjunctBreakConsonant = mempty,
            indicConjunctBreakExtend = HSet.fromList cs,
            indicConjunctBreakLinker = mempty
          }
      Indic_Conjunct_Break_Linker ->
        MkDerivedCorePropertiesI
          { indicConjunctBreakConsonant = mempty,
            indicConjunctBreakExtend = mempty,
            indicConjunctBreakLinker = HSet.fromList cs
          }
      where
        cs = DB.Utils.charRange c mC

-- | Parses a bytestring to a unicode property and char range.
bsToProp :: ByteString -> Maybe (DerivedCoreProperty, Char, Maybe Char)
bsToProp bs = do
  (c1, mC2, r1) <-
    first Just <$> DB.Utils.parseCodePointRange bs
      <|> (\(c, b) -> (c, Nothing, b)) <$> DB.Utils.parseCodePoint bs

  r2 <- DB.Utils.parseSemiColon r1

  (prop, _) <- parseDerivedCoreProperty r2

  pure $ (prop, c1, mC2)

parseDerivedCoreProperty :: ByteString -> Maybe (DerivedCoreProperty, ByteString)
parseDerivedCoreProperty =
  DB.Utils.parseFirst
    [ pIndic_Conjunct_Break_Consonant,
      pIndic_Conjunct_Break_Extend,
      pIndic_Conjunct_Break_Linker
    ]
  where
    pIndic_Conjunct_Break_Consonant =
      pIndic_Conjunct_Break
        Indic_Conjunct_Break_Consonant
        "Consonant"

    pIndic_Conjunct_Break_Extend =
      pIndic_Conjunct_Break
        Indic_Conjunct_Break_Extend
        "Extend"

    pIndic_Conjunct_Break_Linker =
      pIndic_Conjunct_Break
        Indic_Conjunct_Break_Linker
        "Linker"

    pIndic_Conjunct_Break cons ty bs = do
      r1 <- DB.Utils.stripStart <$> DB.Utils.stringExact "InCB;" bs
      r2 <- DB.Utils.stripStart <$> DB.Utils.stringExact ty r1
      pure (cons, r2)

data DerivedCorePropertiesE = MkDerivedCorePropertiesE
  { propType :: DerivedCoreProperty,
    expected :: Int,
    actual :: Int
  }
  deriving stock (Eq, Show)

instance Exception DerivedCorePropertiesE where
  displayException ex =
    mconcat
      [ "Derived_Core_Property parse '",
        show ex.propType,
        "' failure. Expected ",
        show ex.expected,
        ", received ",
        show ex.actual
      ]
