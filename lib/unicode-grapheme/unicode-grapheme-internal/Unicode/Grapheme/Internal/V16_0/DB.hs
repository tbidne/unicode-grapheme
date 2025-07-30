{-# LANGUAGE TemplateHaskell #-}

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
    EmojiData (MkEmojiData),
    GraphemeBreakProperties (MkGraphemeBreakProperties),
    Properties
      ( MkProperties,
        derivedCoreProperties,
        emojiData,
        graphemeBreakProperties
      ),
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
                    Generated.derivedCore_IndicConjunctBreak_Consonant,
                  indicConjunctBreakExtend =
                    Generated.derivedCore_IndicConjunctBreak_Extend,
                  indicConjunctBreakLinker =
                    Generated.derivedCore_IndicConjunctBreak_Linker
                },
            emojiData = MkEmojiData Generated.extendedPictographic,
            graphemeBreakProperties =
              MkGraphemeBreakProperties Generated.graphemeBreakProperties
          }
    }
