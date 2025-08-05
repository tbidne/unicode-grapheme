{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.GraphemeBreakProperty
  ( generateData,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Foldable qualified as F
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Text.Builder.Linear (Builder)
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp)
import Unicode.Grapheme.Generator.DB.GraphemeClusterBreak
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
import Unicode.Grapheme.Generator.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Generator.Utils
  ( PropParser,
    PropertyAssertionE
      ( MkPropertyAssertionE,
        actual,
        expected,
        propTypeName,
        propValue,
        version
      ),
  )
import Unicode.Grapheme.Generator.Utils qualified as Utils
import Unicode.Grapheme.Generator.Version (UnicodeVersion)

type Assertions = HashMap GraphemeClusterBreak Int

type GraphemeBreakProps = Seq (Char, Maybe Char, GraphemeClusterBreak)

generateData :: Maybe OsPath -> Assertions -> UnicodeVersion -> IO Builder
generateData mDataDir asserts uvers = do
  props <- readUnicodeDataIO mDataDir asserts uvers
  pure $
    Utils.serializeCodePointsTuple
      "graphemeBreakProperties"
      "GraphemeClusterBreak"
      props

-- | Reads derived core properties corresponding to the given unicode version.
readUnicodeDataIO ::
  Maybe OsPath ->
  Assertions ->
  UnicodeVersion ->
  IO GraphemeBreakProps
readUnicodeDataIO mDataDir asserts uvers = do
  bs <- FileIO.readFile' path
  let props =
        Utils.parseProps
          propParsers
          go
          Empty
          bs

  checkAsserts props

  pure props
  where
    path =
      Utils.mkUnicodePath mDataDir uvers [osp|GraphemeBreakProperty.txt|]

    go acc (p, c, mC) = acc :|> (c, mC, p)

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
          MkPropertyAssertionE
            { actual = nothingToZero actual,
              expected = nothingToZero expected,
              propTypeName = "Grapheme_Cluster_Break",
              propValue = show gcb,
              version = uvers
            }

    nothingToZero = fromMaybe 0

countMap :: GraphemeBreakProps -> HashMap GraphemeClusterBreak Int
countMap = F.foldl' f mempty
  where
    f ::
      HashMap GraphemeClusterBreak Int ->
      (Char, Maybe Char, GraphemeClusterBreak) ->
      HashMap GraphemeClusterBreak Int
    f acc (c, mC, gcb) = case HMap.lookup gcb acc of
      Nothing -> HMap.insert gcb m acc
      Just !n -> HMap.insert gcb (m + n) acc
      where
        m = Utils.countCodePoint (c, mC)

propParsers :: [PropParser GraphemeClusterBreak]
propParsers =
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
