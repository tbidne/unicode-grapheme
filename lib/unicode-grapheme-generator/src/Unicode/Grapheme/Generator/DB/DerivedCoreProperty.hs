{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.DerivedCoreProperty
  ( generateData,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
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

-- | Derived properties we care about.
data DerivedCoreProperty
  = Indic_Conjunct_Break_Consonant
  | Indic_Conjunct_Break_Extend
  | Indic_Conjunct_Break_Linker
  deriving stock (Eq, Show)

type CodePoints = Seq (Char, Maybe Char)

type DerivedCoreProps = (CodePoints, CodePoints, CodePoints)

type Assertions = (Int, Int, Int)

generateData :: Maybe OsPath -> Assertions -> UnicodeVersion -> IO Builder
generateData mDataDir asserts uvers = do
  (cs, es, ls) <- readUnicodeDataIO mDataDir asserts uvers

  pure $
    -- Not T.unlines as we do not want the trailing newline.
    Utils.unlinesb
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
  let props =
        Utils.parseProps
          propParsers
          go
          (Empty, Empty, Empty)
          bs

  checkAsserts props

  pure props
  where
    path =
      Utils.mkUnicodePath mDataDir uvers [osp|DerivedCoreProperties.txt|]

    go (cs, es, ls) (p, c, mC) = case p of
      Indic_Conjunct_Break_Consonant -> (cs :|> (c, mC), es, ls)
      Indic_Conjunct_Break_Extend -> (cs, es :|> (c, mC), ls)
      Indic_Conjunct_Break_Linker -> (cs, es, ls :|> (c, mC))

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
    checkAssert propValue expected derived = do
      let actual = Utils.countCodePoints derived
      unless (expected == actual) $
        throwIO $
          MkPropertyAssertionE
            { actual,
              expected,
              propTypeName = "Derived_Core_Property",
              propValue = show propValue,
              version = uvers
            }

propParsers :: [PropParser DerivedCoreProperty]
propParsers =
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
