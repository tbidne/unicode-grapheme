{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.EmojiData
  ( generateData,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Text.Builder.Linear (Builder)
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp)
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

-- | Emoji properties we care about.
data EmojiDataProperty
  = Emoji_Presentation
  | Extended_Pictographic
  deriving stock (Eq, Show)

type Assertions = (Int, Int)

type CodePoints = Seq (Char, Maybe Char)

type EmojiDataProps = (CodePoints, CodePoints)

generateData :: Maybe OsPath -> Assertions -> UnicodeVersion -> IO Builder
generateData mDataDir asserts uvers = do
  (pre, pic) <- readUnicodeDataIO mDataDir asserts uvers
  pure $
    Utils.unlinesb
      [ Utils.serializeCodePoints "emojiPresentation" pre,
        Utils.serializeCodePoints "extendedPictographic" pic
      ]

-- | Reads emoji data corresponding to the given unicode version.
readUnicodeDataIO ::
  Maybe OsPath ->
  Assertions ->
  UnicodeVersion ->
  IO EmojiDataProps
readUnicodeDataIO mDataDir (epre, epic) uvers = do
  bs <- FileIO.readFile' path
  let props =
        Utils.parseProps
          propParsers
          go
          (Empty, Empty)
          bs

  checkAsserts props

  pure props
  where
    path =
      Utils.mkUnicodePath mDataDir uvers [osp|emoji-data.txt|]

    go (pre, pic) (p, c, mC) = case p of
      Emoji_Presentation -> (pre :|> (c, mC), pic)
      Extended_Pictographic -> (pre, pic :|> (c, mC))

    checkAsserts :: EmojiDataProps -> IO ()
    checkAsserts (pre, pic) = do
      checkAssert
        Emoji_Presentation
        epre
        pre

      checkAssert
        Extended_Pictographic
        epic
        pic

    checkAssert :: EmojiDataProperty -> Int -> CodePoints -> IO ()
    checkAssert propValue expected cats = do
      let actual = Utils.countCodePoints cats
      unless (expected == actual) $
        throwIO $
          MkPropertyAssertionE
            { actual,
              expected,
              propTypeName = "Emoji_Data",
              propValue = show propValue,
              version = uvers
            }

propParsers :: [PropParser EmojiDataProperty]
propParsers =
  [ pEmoji_Presentation,
    pExtended_Pictographic
  ]
  where
    pEmoji_Presentation =
      mkCons Emoji_Presentation
        . BS.stripPrefix "Emoji_Presentation"

    pExtended_Pictographic =
      mkCons Extended_Pictographic
        . BS.stripPrefix "Extended_Pictographic#"

    mkCons ::
      EmojiDataProperty ->
      Maybe ByteString ->
      Maybe (EmojiDataProperty, ByteString)
    mkCons c = fmap ((c,) . Parsing.stripStart)
