{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.GraphemeBreakProperty
  ( generateData,
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
import Data.Text (Text)
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp)
import Unicode.Grapheme.Common.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak
      ( GraphemeClusterBreak_CR,
        GraphemeClusterBreak_Control,
        GraphemeClusterBreak_Extend,
        GraphemeClusterBreak_L,
        GraphemeClusterBreak_LF,
        GraphemeClusterBreak_LV,
        GraphemeClusterBreak_LVT,
        GraphemeClusterBreak_Prepend,
        GraphemeClusterBreak_Regional_Indicator,
        GraphemeClusterBreak_SpacingMark,
        GraphemeClusterBreak_T,
        GraphemeClusterBreak_V,
        GraphemeClusterBreak_ZWJ
      ),
  )
import Unicode.Grapheme.Common.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Common.Utils qualified as Common.Utils
import Unicode.Grapheme.Common.Version (UnicodeVersion)
import Unicode.Grapheme.Generator.Utils qualified as Utils

type Assertions = HashMap GraphemeClusterBreak Int

type GraphemeBreakProps = [(Char, GraphemeClusterBreak)]

generateData :: Maybe OsPath -> Assertions -> UnicodeVersion -> IO Text
generateData mDataDir asserts uvers = do
  props <- readUnicodeDataIO mDataDir asserts uvers
  pure $ Utils.mkCharMap "graphemeBreakProperties" props

-- | Reads derived core properties corresponding to the given unicode version.
readUnicodeDataIO ::
  Maybe OsPath ->
  Assertions ->
  UnicodeVersion ->
  IO GraphemeBreakProps
readUnicodeDataIO mDataDir asserts uvers = do
  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      props = F.foldl' lineToDerivedProps [] ls

  checkAsserts props

  pure props
  where
    path =
      Common.Utils.mkUnicodePath mDataDir uvers [osp|GraphemeBreakProperty.txt|]

    checkAsserts :: GraphemeBreakProps -> IO ()
    checkAsserts props = do
      let propCount = countMap props
      F.for_ [minBound .. maxBound] $ \gcb -> do
        checkAssert gcb propCount

    checkAssert :: GraphemeClusterBreak -> HashMap GraphemeClusterBreak Int -> IO ()
    checkAssert gcb actualMap = do
      let expected = HMap.lookup gcb asserts
          actual = HMap.lookup gcb actualMap
      unless (expected == actual) $
        throwIO $
          MkGraphemeBreakPropertiesE
            { gcb,
              expected,
              actual
            }

countMap :: [(Char, GraphemeClusterBreak)] -> HashMap GraphemeClusterBreak Int
countMap = F.foldl' f mempty
  where
    f ::
      HashMap GraphemeClusterBreak Int ->
      (Char, GraphemeClusterBreak) ->
      HashMap GraphemeClusterBreak Int
    f acc (_, gcb) = case HMap.lookup gcb acc of
      Nothing -> HMap.insert gcb 1 acc
      Just !n -> HMap.insert gcb (n + 1) acc

lineToDerivedProps :: GraphemeBreakProps -> ByteString -> GraphemeBreakProps
lineToDerivedProps acc bs = case bsToProp bs of
  Nothing -> acc
  Just (p, c, mC) -> zip cs (repeat p) <> acc
    where
      cs = Parsing.charRange c mC

-- | Parses a bytestring to a unicode property and char range.
bsToProp :: ByteString -> Maybe (GraphemeClusterBreak, Char, Maybe Char)
bsToProp bs = do
  (c1, mC2, r1) <-
    first Just <$> Parsing.parseCodePointRange bs
      <|> (\(c, b) -> (c, Nothing, b)) <$> Parsing.parseCodePoint bs

  r2 <- Parsing.parseSemiColon r1

  (prop, _) <- parseGraphemeBreakProperty r2

  pure $ (prop, c1, mC2)

parseGraphemeBreakProperty :: ByteString -> Maybe (GraphemeClusterBreak, ByteString)
parseGraphemeBreakProperty =
  Parsing.parseFirst
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
    pPrepend = mkCons GraphemeClusterBreak_Prepend . Parsing.stringExact "Prepend"
    pCR = mkCons GraphemeClusterBreak_CR . Parsing.stringExact "CR"
    pLF = mkCons GraphemeClusterBreak_LF . Parsing.stringExact "LF"
    pControl = mkCons GraphemeClusterBreak_Control . Parsing.stringExact "Control"
    pExtend = mkCons GraphemeClusterBreak_Extend . Parsing.stringExact "Extend"
    pRegional_Indicator = mkCons GraphemeClusterBreak_Regional_Indicator . Parsing.stringExact "Regional_Indicator"
    pSpacingMark = mkCons GraphemeClusterBreak_SpacingMark . Parsing.stringExact "SpacingMark"
    pL = mkCons GraphemeClusterBreak_L . Parsing.stringExact "L"
    pV = mkCons GraphemeClusterBreak_V . Parsing.stringExact "V"
    pT = mkCons GraphemeClusterBreak_T . Parsing.stringExact "T"
    pLV = mkCons GraphemeClusterBreak_LV . Parsing.stringExact "LV"
    pLVT = mkCons GraphemeClusterBreak_LVT . Parsing.stringExact "LVT"
    pZWJ = mkCons GraphemeClusterBreak_ZWJ . Parsing.stringExact "ZWJ"

    mkCons ::
      GraphemeClusterBreak ->
      Maybe ByteString ->
      Maybe (GraphemeClusterBreak, ByteString)
    mkCons c = fmap ((c,) . Parsing.stripStart)

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
