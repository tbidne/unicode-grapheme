{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.DerivedEastAsianWidth
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
import Unicode.Grapheme.Common.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Common.Utils qualified as Common.Utils
import Unicode.Grapheme.Common.Version (UnicodeVersion)
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
      Common.Utils.mkUnicodePath mDataDir uvers [osp|DerivedEastAsianWidth.txt|]

    go (fs, ws) (p, c, mC) = case p of
      DerivedEastAsian_Full -> (fs :|> (c, mC), ws)
      DerivedEastAsian_Wide -> (fs, ws :|> (c, mC))

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
    checkAssert propValue expected cats = do
      let actual = Utils.countCodePoints cats
      unless (expected == actual) $
        throwIO $
          MkPropertyAssertionE
            { actual,
              expected,
              propTypeName = "Derived_East_Asian_Width",
              propValue = show propValue,
              version = uvers
            }

-- FIXME: According to the derived files, there are some ~60k "missing" values
-- not in the standard spot. First, verify that the missing values listed at
-- the top are in fact the missing 60k. Then, figure out if it matters.
propParsers :: [PropParser DerivedEastAsianWidthProperty]
propParsers =
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
