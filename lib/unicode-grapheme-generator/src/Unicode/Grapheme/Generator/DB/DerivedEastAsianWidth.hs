{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.DerivedEastAsianWidth
  ( generateData,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Exception (Exception (displayException), throwIO)
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.Foldable qualified as F
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder)
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp)
import Unicode.Grapheme.Common.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Common.Utils qualified as Common.Utils
import Unicode.Grapheme.Common.Version (UnicodeVersion)
import Unicode.Grapheme.Common.Version qualified as Version
import Unicode.Grapheme.Generator.Utils qualified as Utils

-- | Emoji properties we care about.
data DerivedEastAsianWidthProperty
  = DerivedEastAsian_Full
  | DerivedEastAsian_Wide
  deriving stock (Eq, Show)

type Assertions = (Int, Int)

type CodePoints = Seq (Char, Maybe Char)

type DerivedEastAsianWidthProps = (CodePoints, CodePoints)

generateData :: Maybe OsPath -> Assertions -> UnicodeVersion -> IO Builder
generateData mDataDir asserts uvers = do
  (fs, ws) <- readUnicodeDataIO mDataDir asserts uvers
  pure $
    Utils.unlinesb
      [ -- Both Fullwidth and Wide are the same as we are concerned
        -- ("wide" hence size 2), but we separate them until now so we can
        -- make the asserts a little more transparent. At this point we can
        -- combine them for a single function.
        Utils.serializeCodePoints "derivedEastAsianWide" (fs <> ws)
      ]

-- | Reads emoji data corresponding to the given unicode version.
readUnicodeDataIO ::
  Maybe OsPath ->
  Assertions ->
  UnicodeVersion ->
  IO DerivedEastAsianWidthProps
readUnicodeDataIO mDataDir (epre, epic) uvers = do
  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      props = F.foldl' lineToDerivedProps (Empty, Empty) ls

  checkAsserts props

  pure props
  where
    path =
      Common.Utils.mkUnicodePath mDataDir uvers [osp|DerivedEastAsianWidth.txt|]

    checkAsserts :: DerivedEastAsianWidthProps -> IO ()
    checkAsserts (fs, ws) = do
      checkAssert
        DerivedEastAsian_Full
        epre
        fs

      checkAssert
        DerivedEastAsian_Wide
        epic
        ws

    checkAssert :: DerivedEastAsianWidthProperty -> Int -> CodePoints -> IO ()
    checkAssert propType expected cats = do
      let actual = Utils.countCodePoints cats
      unless (expected == actual) $
        throwIO $
          MkEmojiDataE
            { version = uvers,
              propType,
              expected,
              actual
            }

lineToDerivedProps :: DerivedEastAsianWidthProps -> ByteString -> DerivedEastAsianWidthProps
lineToDerivedProps acc@(fs, ws) bs = case bsToProp bs of
  Nothing -> acc
  Just (DerivedEastAsian_Full, c, mC) -> (fs :|> (c, mC), ws)
  Just (DerivedEastAsian_Wide, c, mC) -> (fs, ws :|> (c, mC))

-- | Parses a bytestring to a unicode property and char range.
bsToProp :: ByteString -> Maybe (DerivedEastAsianWidthProperty, Char, Maybe Char)
bsToProp bs = do
  (c1, mC2, r1) <-
    first Just <$> Parsing.parseCodePointRange bs
      <|> (\(c, b) -> (c, Nothing, b)) <$> Parsing.parseCodePoint bs

  r2 <- Parsing.parseSemiColon r1

  (prop, _) <- parseEmojiDataProperty r2

  pure (prop, c1, mC2)

-- FIXME: According to the derived files, there are some ~60k "missing" values
-- not in the standard spot. First, verify that the missing values listed at
-- the top are in fact the missing 60k. Then, figure out if it matters.
parseEmojiDataProperty :: ByteString -> Maybe (DerivedEastAsianWidthProperty, ByteString)
parseEmojiDataProperty =
  Parsing.parseFirst
    [ pFull,
      pWide
    ]
  where
    pFull =
      mkCons DerivedEastAsian_Full
        . BS.stripPrefix "F"

    pWide =
      mkCons DerivedEastAsian_Wide
        . BS.stripPrefix "W"

    mkCons ::
      DerivedEastAsianWidthProperty ->
      Maybe ByteString ->
      Maybe (DerivedEastAsianWidthProperty, ByteString)
    mkCons c = fmap ((c,) . Parsing.stripStart)

data EmojiDataE = MkEmojiDataE
  { version :: UnicodeVersion,
    propType :: DerivedEastAsianWidthProperty,
    expected :: Int,
    actual :: Int
  }
  deriving stock (Eq, Show)

instance Exception EmojiDataE where
  displayException ex =
    mconcat
      [ T.unpack (Version.displayVersion ex.version),
        ": Emoji_Data property parse '",
        show ex.propType,
        "' failure. Expected ",
        show ex.expected,
        ", received ",
        show ex.actual
      ]
