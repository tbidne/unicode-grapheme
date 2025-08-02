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

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Foldable qualified as F
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (catMaybes)
import Data.Maybe qualified as M
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import System.File.OsPath qualified as FileIO
import System.OsPath (osp, (</>))
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
    rules :: [Text],
    values :: [GraphemeBreakTestValue]
  }
  deriving stock (Eq, Show)

lineToExpected :: GraphemeBreakTestLine -> [Text]
lineToExpected =
  F.toList
    . fmap (T.pack . F.toList)
    . F.foldl' f Empty
    . (.values)
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
    . M.mapMaybe f
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
      L.unwords $ F.toList $ displayGraphemeBreakTestValue <$> line.values
    ]

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
  let dataDir = [osp|test|] </> [osp|data|]

      path =
        Common.Utils.mkUnicodePath
          (Just dataDir)
          vers
          [osp|GraphemeBreakTest.txt|]

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
  (valuesRaw, rest) <- parseSomeValues bs
  let values = stripLine valuesRaw
  rules <- F.toList <$> parseSomeRules rest
  pure $
    MkGraphemeBreakTestLine
      { lineNum,
        rules,
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

parseSomeValues :: ByteString -> Maybe (NonEmpty GraphemeBreakTestValue, ByteString)
parseSomeValues bs = do
  let (vs, rest) = go ([], bs)
  (,rest) <$> nonEmpty (L.reverse vs)
  where
    go ::
      ([GraphemeBreakTestValue], ByteString) ->
      ([GraphemeBreakTestValue], ByteString)
    go acc@(_, "") = acc
    go acc@(vs, rs) = case parseGraphemeBreakTestValue rs of
      Nothing -> acc
      Just (v, rest) -> go (v : vs, rest)

parseSomeRules :: ByteString -> Maybe (NonEmpty Text)
parseSomeRules = nonEmpty . go
  where
    go "" = []
    go bs = case parseRule bs of
      Nothing -> []
      Just (r, rest) -> r : go rest

parseGraphemeBreakTestValue :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseGraphemeBreakTestValue =
  Parsing.parseFirst
    [ parseBreak,
      parseNoBreak,
      parseChar
    ]

parseRule :: ByteString -> Maybe (Text, ByteString)
parseRule bs = do
  -- parse until l bracket
  let (_, r1) = BS.break (== lbracket) bs
  r2 <- Parsing.parseW8NoStrip lbracket r1

  -- parse num
  let (n1, r3) = BS.span Parsing.isDigit r2
  guard $ (not . BS.null) n1

  r4 <- Parsing.parseDot r3

  -- parse after decimal point
  let (n2, r5) = BS.span Parsing.isDigit r4
  guard $ (not . BS.null) n2
  r6 <- Parsing.parseW8NoStrip rbracket r5
  (,r6) <$> case (n1, n2) of
    -- For whatever reason, the rules marked GB1 and GB2 are known as
    -- 0.2 and 0.3 in the test files. The spec says:
    --
    --     The rules “sot ÷”, “÷ eot”, and “÷ Any” are added mechanically
    --     and have artificial numbers.
    ("0", "2") -> pure "GB1"
    ("0", "3") -> pure "GB2"
    ("3", "0") -> pure "GB3"
    ("4", "0") -> pure "GB4"
    ("5", "0") -> pure "GB5"
    ("6", "0") -> pure "GB6"
    ("7", "0") -> pure "GB7"
    ("8", "0") -> pure "GB8"
    ("9", "0") -> pure "GB9"
    ("9", "1") -> pure "GB9a"
    ("9", "2") -> pure "GB9b"
    ("9", "3") -> pure "GB9c"
    ("11", "0") -> pure "GB11"
    ("12", "0") -> pure "GB12_13"
    ("13", "0") -> pure "GB12_13"
    ("999", "0") -> pure "GB999"
    other -> error $ "Unknown rule: " ++ show other
  where
    lbracket = 0x5B
    rbracket = 0x5D

parseChar :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseChar = fmap (first ValueChar) . Parsing.parseCodePoint

-- ÷
parseBreak :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseBreak bs = do
  bs1 <- Parsing.parseW8 0xC3 bs
  bs2 <- Parsing.parseW8 0xB7 bs1
  pure (ValueBreak, bs2)

-- ×
parseNoBreak :: ByteString -> Maybe (GraphemeBreakTestValue, ByteString)
parseNoBreak bs = do
  bs1 <- Parsing.parseW8 0xC3 bs
  bs2 <- Parsing.parseW8 0x97 bs1
  pure (ValueNoBreak, bs2)
