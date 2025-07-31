{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.DerivedCoreProperty
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
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Text (Text)
import System.File.OsPath qualified as FileIO
import System.OsPath (OsPath, osp)
import Unicode.Grapheme.Common.DB.Parsing qualified as Parsing
import Unicode.Grapheme.Common.Utils qualified as Common.Utils
import Unicode.Grapheme.Common.Version (UnicodeVersion)
import Unicode.Grapheme.Generator.Utils qualified as Utils

-- | Derived properties we care about.
data DerivedCoreProperty
  = Indic_Conjunct_Break_Consonant
  | Indic_Conjunct_Break_Extend
  | Indic_Conjunct_Break_Linker
  deriving stock (Eq, Show)

type CodePoints = Seq (Char, Maybe Char)

type DerivedCoreProps = (CodePoints, CodePoints, CodePoints)

type Assertions = (Int, Int, Int)

generateData :: Maybe OsPath -> Assertions -> UnicodeVersion -> IO Text
generateData mDataDir asserts uvers = do
  (cs, es, ls) <- readUnicodeDataIO mDataDir asserts uvers

  pure $
    -- Not T.unlines as we do not want the trailing newline.
    Utils.tunlines
      [ Utils.serializeCodePoints "derivedCore_IndicConjunctBreak_Consonant" cs,
        Utils.serializeCodePoints "derivedCore_IndicConjunctBreak_Extend" es,
        Utils.serializeCodePoints "derivedCore_IndicConjunctBreak_Linker" ls
      ]

-- | Reads derived core properties corresponding to the given unicode version.
readUnicodeDataIO ::
  Maybe OsPath ->
  Assertions ->
  UnicodeVersion ->
  IO DerivedCoreProps
readUnicodeDataIO mDataDir (icbConsonant, icbExtend, icbLinker) uvers = do
  bs <- FileIO.readFile' path
  let ls = C8.lines bs
      props = F.foldl' lineToDerivedProps (Empty, Empty, Empty) ls

  checkAsserts props

  pure props
  where
    path =
      Common.Utils.mkUnicodePath mDataDir uvers [osp|DerivedCoreProperties.txt|]

    checkAsserts :: DerivedCoreProps -> IO ()
    checkAsserts (c, e, l) = do
      checkAssert
        Indic_Conjunct_Break_Consonant
        icbConsonant
        c

      checkAssert
        Indic_Conjunct_Break_Extend
        icbExtend
        e

      checkAssert
        Indic_Conjunct_Break_Linker
        icbLinker
        l

    checkAssert :: DerivedCoreProperty -> Int -> CodePoints -> IO ()
    checkAssert propType expected derived = do
      let actual = Utils.countCodePoints derived
      unless (expected == actual) $
        throwIO $
          MkDerivedCorePropertiesE
            { propType,
              expected,
              actual
            }

lineToDerivedProps :: DerivedCoreProps -> ByteString -> DerivedCoreProps
lineToDerivedProps acc@(cs, es, ls) bs = case bsToProp bs of
  Nothing -> acc
  Just (Indic_Conjunct_Break_Consonant, c, mC) -> (cs :|> (c, mC), es, ls)
  Just (Indic_Conjunct_Break_Extend, c, mC) -> (cs, es :|> (c, mC), ls)
  Just (Indic_Conjunct_Break_Linker, c, mC) -> (cs, es, ls :|> (c, mC))

-- | Parses a bytestring to a unicode property and char range.
bsToProp :: ByteString -> Maybe (DerivedCoreProperty, Char, Maybe Char)
bsToProp bs = do
  (c1, mC2, r1) <-
    first Just <$> Parsing.parseCodePointRange bs
      <|> (\(c, b) -> (c, Nothing, b)) <$> Parsing.parseCodePoint bs

  r2 <- Parsing.parseSemiColon r1

  (prop, _) <- parseDerivedCoreProperty r2

  pure $ (prop, c1, mC2)

parseDerivedCoreProperty :: ByteString -> Maybe (DerivedCoreProperty, ByteString)
parseDerivedCoreProperty =
  Parsing.parseFirst
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
      r1 <- Parsing.stripStart <$> Parsing.stringExact "InCB;" bs
      r2 <- Parsing.stripStart <$> Parsing.stringExact ty r1
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
