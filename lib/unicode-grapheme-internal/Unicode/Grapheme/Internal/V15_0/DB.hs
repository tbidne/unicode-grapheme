{-# LANGUAGE TemplateHaskell #-}

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
  )
import Unicode.Grapheme.Internal.V15_0.DB.Generated qualified as Generated

newtype UnicodeDatabase = MkUnicodeDatabase
  { unUnicodeDatabase :: Properties
  }
  deriving stock (Eq, Show)

database :: UnicodeDatabase
database =
  MkUnicodeDatabase
    { unUnicodeDatabase =
        MkProperties
          { derivedCoreProperties = mempty,
            emojiData = MkEmojiData Generated.extendedPictographic,
            graphemeBreakProperties =
              MkGraphemeBreakProperties Generated.graphemeBreakProperties
          }
    }
