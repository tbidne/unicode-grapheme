module Unicode.Grapheme.Internal.V15_0.DB
  ( UnicodeDatabase (..),
    database,
  )
where

import Unicode.Grapheme.Internal.DB.Properties
  ( EmojiData (MkEmojiData),
    GraphemeBreakProperties (MkGraphemeBreakProperties),
    Properties
      ( MkProperties,
        derivedCoreProperties,
        emojiData,
        graphemeBreakProperties
      ),
    mkCharMap,
    mkCharSet,
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
            emojiData =
              MkEmojiData $ mkCharSet Generated.extendedPictographic,
            graphemeBreakProperties =
              MkGraphemeBreakProperties $
                mkCharMap Generated.graphemeBreakProperties
          }
    }
