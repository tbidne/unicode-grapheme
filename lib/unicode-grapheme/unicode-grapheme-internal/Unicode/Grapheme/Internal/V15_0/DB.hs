module Unicode.Grapheme.Internal.V15_0.DB
  ( UnicodeDatabase (..),
    database,
  )
where

import Unicode.Grapheme.Internal.DB.Properties
  ( DerivedEastAsianWidth (MkDerivedEastAsianWidth, derivedEastAsianWide),
    EmojiData (MkEmojiData, emojiPresentation, extendedPictographic),
    GraphemeBreakProperties (MkGraphemeBreakProperties),
    Properties
      ( MkProperties,
        derivedCoreProperties,
        derivedEastAsianWidth,
        emojiData,
        graphemeBreakProperties
      ),
    mkCharBitVec,
    mkCharVecGCB,
  )
import Unicode.Grapheme.Internal.V15_0.DB.Generated qualified as Generated

newtype UnicodeDatabase = MkUnicodeDatabase
  { unUnicodeDatabase :: Properties
  }

database :: UnicodeDatabase
database =
  MkUnicodeDatabase
    { unUnicodeDatabase =
        MkProperties
          { derivedCoreProperties = mempty,
            derivedEastAsianWidth =
              MkDerivedEastAsianWidth
                { derivedEastAsianWide = mkCharBitVec Generated.derivedEastAsianWide
                },
            emojiData =
              MkEmojiData
                { emojiPresentation =
                    mkCharBitVec Generated.emojiPresentation,
                  extendedPictographic =
                    mkCharBitVec Generated.extendedPictographic
                },
            graphemeBreakProperties =
              MkGraphemeBreakProperties $
                mkCharVecGCB Generated.graphemeBreakProperties
          }
    }
