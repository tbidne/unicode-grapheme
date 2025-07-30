{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.EmojiData
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
import Data.Text (Text)
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp)
import Unicode.Grapheme.Common.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Common.Utils qualified as Common.Utils
import Unicode.Grapheme.Common.Version (UnicodeVersion)
import Unicode.Grapheme.Generator.Utils qualified as Utils

-- | Emoji properties we care about.
data EmojiDataProperty
  = Extended_Pictographic
  deriving stock (Eq, Show)

generateData :: Maybe OsPath -> Int -> UnicodeVersion -> IO Text
generateData mDataDir expectedPictographics uvers = do
  cs <- readUnicodeDataIO mDataDir expectedPictographics uvers
  pure $ Utils.mkCharSet "extendedPictographic" cs

-- | Reads emoji data corresponding to the given unicode version.
readUnicodeDataIO ::
  Maybe OsPath ->
  Int ->
  UnicodeVersion ->
  IO [Char]
readUnicodeDataIO mDataDir expectedPictographics uvers = do
  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      props = foldMap lineToDerivedProps ls

  checkAsserts props

  pure props
  where
    path =
      Common.Utils.mkUnicodePath mDataDir uvers [osp|emoji-data.txt|]

    checkAsserts :: [Char] -> IO ()
    checkAsserts cats = do
      checkAssert
        Extended_Pictographic
        expectedPictographics
        cats

    checkAssert :: EmojiDataProperty -> Int -> [Char] -> IO ()
    checkAssert propType expected cats = do
      let actual = length cats
      unless (expected == actual) $
        throwIO $
          MkEmojiDataE
            { propType,
              expected,
              actual
            }

lineToDerivedProps :: ByteString -> [Char]
lineToDerivedProps bs = maybe mempty f (bsToProp bs)
  where
    f (p, c, mC) = case p of
      Extended_Pictographic -> cs
      where
        cs = Parsing.charRange c mC

-- | Parses a bytestring to a unicode property and char range.
bsToProp :: ByteString -> Maybe (EmojiDataProperty, Char, Maybe Char)
bsToProp bs = do
  (c1, mC2, r1) <-
    first Just <$> Parsing.parseCodePointRange bs
      <|> (\(c, b) -> (c, Nothing, b)) <$> Parsing.parseCodePoint bs

  r2 <- Parsing.parseSemiColon r1

  (prop, _) <- parseEmojiDataProperty r2

  pure $ (prop, c1, mC2)

parseEmojiDataProperty :: ByteString -> Maybe (EmojiDataProperty, ByteString)
parseEmojiDataProperty =
  Parsing.parseFirst
    [ pExtended_Pictographic
    ]
  where
    pExtended_Pictographic =
      mkCons Extended_Pictographic
        . BS.stripPrefix "Extended_Pictographic#"

    mkCons ::
      EmojiDataProperty ->
      Maybe ByteString ->
      Maybe (EmojiDataProperty, ByteString)
    mkCons c = fmap ((c,) . Parsing.stripStart)

data EmojiDataE = MkEmojiDataE
  { propType :: EmojiDataProperty,
    expected :: Int,
    actual :: Int
  }
  deriving stock (Eq, Show)

instance Exception EmojiDataE where
  displayException ex =
    mconcat
      [ "Emoji_Data property parse '",
        show ex.propType,
        "' failure. Expected ",
        show ex.expected,
        ", received ",
        show ex.actual
      ]
