module Unicode.Grapheme.Internal.V16_0.DB
  ( UnicodeDatabase (..),
    database,
  )
where

import Unicode.Grapheme.Internal.DB.Properties
  ( DerivedCoreProperties
      ( MkDerivedCoreProperties,
        indicConjunctBreakConsonant,
        indicConjunctBreakExtend,
        indicConjunctBreakLinker
      ),
    DerivedEastAsianWidth (MkDerivedEastAsianWidth, derivedEastAsianWide),
    EmojiData (MkEmojiData, emojiPresentation, extendedPictographic),
    GraphemeBreakProperties (MkGraphemeBreakProperties),
    Properties
      ( MkProperties,
        derivedCoreProperties,
        derivedEastAsianWidth,
        emojiData,
        graphemeBreakProperties
      ),
    mkCharMap,
    mkCharSet,
  )
import Unicode.Grapheme.Internal.V15_1.DB.Generated qualified as Generated

newtype UnicodeDatabase = MkUnicodeDatabase
  { unUnicodeDatabase :: Properties
  }
  deriving stock (Eq, Show)

database :: UnicodeDatabase
database =
  MkUnicodeDatabase
    { unUnicodeDatabase =
        MkProperties
          { derivedCoreProperties =
              MkDerivedCoreProperties
                { indicConjunctBreakConsonant =
                    mkCharSet Generated.derivedCore_IndicConjunctBreak_Consonant,
                  indicConjunctBreakExtend =
                    mkCharSet Generated.derivedCore_IndicConjunctBreak_Extend,
                  indicConjunctBreakLinker =
                    mkCharSet Generated.derivedCore_IndicConjunctBreak_Linker
                },
            derivedEastAsianWidth =
              MkDerivedEastAsianWidth
                { derivedEastAsianWide = mkCharSet Generated.derivedEastAsianWide
                },
            emojiData =
              MkEmojiData
                { emojiPresentation =
                    mkCharSet Generated.emojiPresentation,
                  extendedPictographic =
                    mkCharSet Generated.extendedPictographic
                },
            graphemeBreakProperties =
              MkGraphemeBreakProperties $
                mkCharMap Generated.graphemeBreakProperties
          }
    }
