module Unicode.Grapheme.Internal.V14_0.DB
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
    mkCharMap,
    mkCharSet,
  )
import Unicode.Grapheme.Internal.V14_0.DB.Generated qualified as Generated

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
