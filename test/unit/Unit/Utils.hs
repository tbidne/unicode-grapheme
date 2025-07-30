{-# LANGUAGE QuasiQuotes #-}

module Unit.Utils
  ( GraphemeBreakTestsParams (..),
    versionToParams,
    GraphemeBreakTestLine (..),
    lineToText,
    lineToExpected,
    displayGraphemeBreakTestLine,
    GraphemeBreakTestValue (..),
    readGraphemeBreakTestsParams,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import System.File.OsPath qualified as FileIO
import System.OsPath (osp)
import Unicode.Grapheme.Common.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Common.Utils qualified as Common.Utils
import Unicode.Grapheme.Common.Version
  ( UnicodeVersion
      ( UnicodeVersion_15_0,
        UnicodeVersion_15_1,
        UnicodeVersion_16_0
      ),
  )

data GraphemeBreakTestsParams = MkGraphemeBreakTestsParams
  { lines_15_0 :: NonEmpty GraphemeBreakTestLine,
    lines_15_1 :: NonEmpty GraphemeBreakTestLine,
    lines_16_0 :: NonEmpty GraphemeBreakTestLine
  }
  deriving stock (Eq, Show)

versionToParams :: UnicodeVersion -> GraphemeBreakTestsParams -> [GraphemeBreakTestLine]
versionToParams v params = F.toList $ case v of
  UnicodeVersion_15_0 -> params.lines_15_0
  UnicodeVersion_15_1 -> params.lines_15_1
  UnicodeVersion_16_0 -> params.lines_16_0

-- ÷ 0020 × 0308 ÷ 0020 ÷	#  ÷ [0.2] SPACE (Other) × [9.0] COMBINING DIAERESIS (Extend_ExtCccZwj) ÷ [999.0] SPACE (Other) ÷ [0.3]
data GraphemeBreakTestLine = MkGraphemeBreakTestLine
  { lineNum :: Word16,
    values :: [GraphemeBreakTestValue]
  }
  deriving stock (Eq, Show)

lineToExpected :: GraphemeBreakTestLine -> [Text]
lineToExpected =
  F.toList
    . fmap (T.pack . F.toList)
    . F.foldl' f Empty
    . values
  where
    f :: Seq (Seq Char) -> GraphemeBreakTestValue -> Seq (Seq Char)
    f Empty ValueBreak = Seq.singleton Empty
    f Empty ValueNoBreak = Empty
    f Empty (ValueChar c) = Seq.singleton (Seq.singleton c)
    f (accs :|> acc) ValueBreak = accs :|> acc :|> Empty
    f (accs :|> acc) ValueNoBreak = accs :|> acc
    f (accs :|> acc) (ValueChar c) = accs :|> (acc :|> c)

lineToText :: GraphemeBreakTestLine -> Text
lineToText =
  T.pack
    . catMaybes
    . fmap f
    . F.toList
    . (.values)
  where
    f :: GraphemeBreakTestValue -> Maybe Char
    f (ValueChar c) = Just c
    f _ = Nothing

displayGraphemeBreakTestLine :: GraphemeBreakTestLine -> String
displayGraphemeBreakTestLine line =
  mconcat
    [ show line.lineNum,
      ": ",
      L.intercalate " " $ F.toList $ displayGraphemeBreakTestValue <$> line.values
    ]
  where

data GraphemeBreakTestValue
  = ValueBreak
  | ValueNoBreak
  | ValueChar Char
  deriving stock (Eq, Show)

displayGraphemeBreakTestValue :: GraphemeBreakTestValue -> String
displayGraphemeBreakTestValue ValueBreak = "÷"
displayGraphemeBreakTestValue ValueNoBreak = "×"
displayGraphemeBreakTestValue (ValueChar c) = Parsing.charToHexStringPadN 4 c

readGraphemeBreakTestsParams :: IO GraphemeBreakTestsParams
readGraphemeBreakTestsParams = do
  lines_15_0 <- readGraphemeBreakTestFile UnicodeVersion_15_0
  lines_15_1 <- readGraphemeBreakTestFile UnicodeVersion_15_1
  lines_16_0 <- readGraphemeBreakTestFile UnicodeVersion_16_0

  pure $
    MkGraphemeBreakTestsParams
      { lines_15_0,
        lines_15_1,
        lines_16_0
      }

readGraphemeBreakTestFile :: UnicodeVersion -> IO (NonEmpty GraphemeBreakTestLine)
readGraphemeBreakTestFile vers = do
  let path =
        Common.Utils.mkUnicodePath Nothing vers [osp|GraphemeBreakTest.txt|]

  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      results = catMaybes $ L.zipWith parseGraphemeBreakTestLine [1 ..] ls
  case nonEmpty results of
    Nothing ->
      error $
        mconcat
          [ "Did not parse any entries in ",
            show path
          ]
    Just ns -> pure ns

parseGraphemeBreakTestLine :: Word16 -> ByteString -> Maybe GraphemeBreakTestLine
parseGraphemeBreakTestLine lineNum bs = do
  values <- stripLine <$> parseSome bs
  pure $
    MkGraphemeBreakTestLine
      { lineNum,
        values
      }

-- The unicode test files start/end with breaks/no-breaks, but our text
-- function does not, hence we need to strip these (in practice it seems
-- like only the end of the list is the problem).
stripLine :: NonEmpty GraphemeBreakTestValue -> [GraphemeBreakTestValue]
stripLine =
  L.dropWhileEnd isNonChar
    . L.dropWhile isNonChar
    . F.toList
  where
    isNonChar (ValueChar _) = False
    isNonChar _ = True

parseSome :: ByteString -> Maybe (NonEmpty GraphemeBreakTestValue)
parseSome = nonEmpty . go
  where
    go :: ByteString -> [GraphemeBreakTestValue]
    go "" = []
    go bs = case parseGraphemeBreakTestValue bs of
      Nothing -> []
      Just (v, rest) -> v : go rest

parseGraphemeBreakTestValue :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseGraphemeBreakTestValue =
  Parsing.parseFirst
    [ parseBreak,
      parseNoBreak,
      parseChar
    ]

parseChar :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseChar = fmap (\(c, bs) -> ((ValueChar c), bs)) . Parsing.parseCodePoint

-- ÷
parseBreak :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseBreak bs = do
  bs1 <- Parsing.parseW8 0xC3 bs
  bs2 <- Parsing.parseW8 0xB7 bs1
  pure $ (ValueBreak, bs2)

-- ×
parseNoBreak :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseNoBreak bs = do
  bs1 <- Parsing.parseW8 0xC3 bs
  bs2 <- Parsing.parseW8 0x97 bs1
  pure $ (ValueNoBreak, bs2)
