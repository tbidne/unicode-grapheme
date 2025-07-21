{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Unicode.Internal.DB.Common.GraphemeBreakProperty
  ( -- * Types
    GraphemeBreakPropertiesI (..),
    GraphemeBreakProperties,
    GraphemeBreakPropertyAssertions,
    GraphemeBreakPropertiesF,
    GraphemeClusterBreak (..),

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
import Data.Foldable qualified as F
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Hashable (Hashable)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import System.File.OsPath qualified as FileIO
import System.OsPath (osp)
import Unicode.Internal.DB.Common.Utils
  ( PropertiesIndex (PropertiesAssertions, PropertiesData),
  )
import Unicode.Internal.DB.Common.Utils qualified as Common.Utils
import Unicode.Internal.DB.Utils qualified as DB.Utils
import Unicode.Internal.Version (UnicodeVersion)

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
  deriving stock (Bounded, Enum, Eq, Generic, Lift, Show)
  deriving anyclass (Hashable)

type GraphemeBreakPropertiesF :: PropertiesIndex -> Type
type family GraphemeBreakPropertiesF p where
  GraphemeBreakPropertiesF PropertiesAssertions = HashMap GraphemeClusterBreak Int
  GraphemeBreakPropertiesF PropertiesData = HashMap Char GraphemeClusterBreak

-- | Map for Char -> GraphemeBreakProperty.
type GraphemeBreakPropertiesI :: PropertiesIndex -> Type
data GraphemeBreakPropertiesI p = MkGraphemeBreakPropertiesI
  { unGraphemeBreakPropertiesI :: GraphemeBreakPropertiesF p
  }

instance
  (Semigroup (GraphemeBreakPropertiesF p)) =>
  Semigroup (GraphemeBreakPropertiesI p)
  where
  MkGraphemeBreakPropertiesI a1 <> MkGraphemeBreakPropertiesI b1 =
    MkGraphemeBreakPropertiesI (a1 <> b1)

instance
  (Monoid (GraphemeBreakPropertiesF p)) =>
  Monoid (GraphemeBreakPropertiesI p)
  where
  mempty = MkGraphemeBreakPropertiesI mempty

-- | Type synonym for GraphemeBreakPropertiesI data.
type GraphemeBreakProperties = GraphemeBreakPropertiesI PropertiesData

-- | Type synonym for GraphemeBreakPropertiesI assertions.
type GraphemeBreakPropertyAssertions = GraphemeBreakPropertiesI PropertiesAssertions

deriving stock instance (Eq (GraphemeBreakPropertiesF p)) => Eq (GraphemeBreakPropertiesI p)

deriving stock instance (Lift (GraphemeBreakPropertiesF p)) => Lift (GraphemeBreakPropertiesI p)

deriving stock instance (Show (GraphemeBreakPropertiesF p)) => Show (GraphemeBreakPropertiesI p)

-- | Reads grapheme break properties corresponding to the given unicode version.
readUnicodeDataIO ::
  GraphemeBreakPropertyAssertions ->
  UnicodeVersion ->
  IO GraphemeBreakProperties
readUnicodeDataIO asserts uvers = do
  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      props = foldMap lineToDerivedProps ls

  checkAsserts props

  pure props
  where
    path =
      Common.Utils.mkUnicodePath uvers [osp|GraphemeBreakProperty.txt|]

    assertsMap = asserts.unGraphemeBreakPropertiesI

    checkAsserts :: GraphemeBreakProperties -> IO ()
    checkAsserts props = do
      let propCount = countMap props.unGraphemeBreakPropertiesI
      F.for_ [minBound .. maxBound] $ \gcb -> do
        checkAssert gcb propCount

    checkAssert :: GraphemeClusterBreak -> HashMap GraphemeClusterBreak Int -> IO ()
    checkAssert gcb actualMap = do
      let expected = HMap.lookup gcb assertsMap
          actual = HMap.lookup gcb actualMap

      unless (expected == actual) $
        throwIO $
          MkGraphemeBreakPropertiesE
            { gcb,
              expected,
              actual
            }

countMap :: HashMap Char GraphemeClusterBreak -> HashMap GraphemeClusterBreak Int
countMap = F.foldl' f mempty . HMap.toList
  where
    f ::
      HashMap GraphemeClusterBreak Int ->
      (Char, GraphemeClusterBreak) ->
      HashMap GraphemeClusterBreak Int
    f acc (_, gcb) = case HMap.lookup gcb acc of
      Nothing -> HMap.insert gcb 1 acc
      Just !n -> HMap.insert gcb (n + 1) acc

lineToDerivedProps :: ByteString -> GraphemeBreakProperties
lineToDerivedProps bs = maybe mempty f (bsToProp bs)
  where
    f :: (GraphemeClusterBreak, Char, Maybe Char) -> GraphemeBreakPropertiesI PropertiesData
    f (p, c, mC) =
      MkGraphemeBreakPropertiesI
        { unGraphemeBreakPropertiesI = HMap.fromList $ zip cs (repeat p)
        }
      where
        cs = DB.Utils.charRange c mC

-- | Parses a bytestring to a unicode property and char range.
bsToProp :: ByteString -> Maybe (GraphemeClusterBreak, Char, Maybe Char)
bsToProp bs = do
  (c1, mC2, r1) <-
    first Just <$> DB.Utils.parseCodePointRange bs
      <|> (\(c, b) -> (c, Nothing, b)) <$> DB.Utils.parseCodePoint bs

  r2 <- DB.Utils.parseSemiColon r1

  (prop, _) <- parseGraphemeBreakProperty r2

  pure $ (prop, c1, mC2)

parseGraphemeBreakProperty :: ByteString -> Maybe (GraphemeClusterBreak, ByteString)
parseGraphemeBreakProperty =
  DB.Utils.parseFirst
    [ pPrepend,
      pCR,
      pLF,
      pControl,
      pExtend,
      pRegional_Indicator,
      pSpacingMark,
      pL,
      pV,
      pT,
      pLV,
      pLVT,
      pZWJ
    ]
  where
    pPrepend = mkCons GraphemeClusterBreak_Prepend . DB.Utils.stringExact "Prepend"
    pCR = mkCons GraphemeClusterBreak_CR . DB.Utils.stringExact "CR"
    pLF = mkCons GraphemeClusterBreak_LF . DB.Utils.stringExact "LF"
    pControl = mkCons GraphemeClusterBreak_Control . DB.Utils.stringExact "Control"
    pExtend = mkCons GraphemeClusterBreak_Extend . DB.Utils.stringExact "Extend"
    pRegional_Indicator = mkCons GraphemeClusterBreak_Regional_Indicator . DB.Utils.stringExact "Regional_Indicator"
    pSpacingMark = mkCons GraphemeClusterBreak_SpacingMark . DB.Utils.stringExact "SpacingMark"
    pL = mkCons GraphemeClusterBreak_L . DB.Utils.stringExact "L"
    pV = mkCons GraphemeClusterBreak_V . DB.Utils.stringExact "V"
    pT = mkCons GraphemeClusterBreak_T . DB.Utils.stringExact "T"
    pLV = mkCons GraphemeClusterBreak_LV . DB.Utils.stringExact "LV"
    pLVT = mkCons GraphemeClusterBreak_LVT . DB.Utils.stringExact "LVT"
    pZWJ = mkCons GraphemeClusterBreak_ZWJ . DB.Utils.stringExact "ZWJ"

    mkCons ::
      GraphemeClusterBreak ->
      Maybe ByteString ->
      Maybe (GraphemeClusterBreak, ByteString)
    mkCons c = fmap ((c,) . DB.Utils.stripStart)

data GraphemeBreakPropertiesE = MkGraphemeBreakPropertiesE
  { gcb :: GraphemeClusterBreak,
    expected :: Maybe Int,
    actual :: Maybe Int
  }
  deriving stock (Eq, Show)

instance Exception GraphemeBreakPropertiesE where
  displayException ex =
    mconcat
      [ "Grapheme_Cluster_Break property parse '",
        show ex.gcb,
        "' failure. Expected ",
        showMaybe ex.expected,
        ", received ",
        showMaybe ex.actual
      ]
    where
      showMaybe Nothing = "<none>"
      showMaybe (Just n) = show n
