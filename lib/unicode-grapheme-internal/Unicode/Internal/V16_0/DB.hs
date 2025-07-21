{-# LANGUAGE TemplateHaskell #-}

module Unicode.Internal.V16_0.DB
  ( UnicodeDatabase (..),
    mkUnicodeDatabaseIO,
  )
where

import Data.HashMap.Strict qualified as HMap
import Language.Haskell.TH.Syntax (Lift)
import Unicode.Internal.DB.Common
  ( Properties,
    PropertiesI
      ( MkPropertiesI,
        derivedCoreProperties,
        emojiData,
        graphemeBreakProperties
      ),
    readUnicodeDataIO,
  )
import Unicode.Internal.DB.Common.DerivedCoreProperty
  ( DerivedCorePropertiesI
      ( MkDerivedCorePropertiesI,
        indicConjunctBreakConsonant,
        indicConjunctBreakExtend,
        indicConjunctBreakLinker
      ),
  )
import Unicode.Internal.DB.Common.EmojiData
  ( EmojiDataI (MkEmojiDataI, extendedPictographic),
  )
import Unicode.Internal.DB.Common.GraphemeBreakProperty
  ( GraphemeBreakPropertiesI (MkGraphemeBreakPropertiesI),
    GraphemeBreakPropertyAssertions,
    GraphemeClusterBreak
      ( GraphemeClusterBreak_CR,
        GraphemeClusterBreak_Control,
        GraphemeClusterBreak_Extend,
        GraphemeClusterBreak_L,
        GraphemeClusterBreak_LF,
        GraphemeClusterBreak_LV,
        GraphemeClusterBreak_LVT,
        GraphemeClusterBreak_Prepend,
        GraphemeClusterBreak_Regional_Indicator,
        GraphemeClusterBreak_SpacingMark,
        GraphemeClusterBreak_T,
        GraphemeClusterBreak_V,
        GraphemeClusterBreak_ZWJ
      ),
  )
import Unicode.Internal.Version (UnicodeVersion (UnicodeVersion_16_0))

newtype UnicodeDatabase = MkUnicodeDatabase
  { unUnicodeDatabase :: Properties
  }
  deriving stock (Eq, Lift, Show)

mkUnicodeDatabaseIO :: IO UnicodeDatabase
mkUnicodeDatabaseIO =
  MkUnicodeDatabase <$> readUnicodeDataIO asserts UnicodeVersion_16_0
  where
    asserts =
      MkPropertiesI
        { derivedCoreProperties =
            MkDerivedCorePropertiesI
              { indicConjunctBreakConsonant = 240,
                indicConjunctBreakExtend = 2192,
                indicConjunctBreakLinker = 6
              },
          emojiData =
            MkEmojiDataI
              { extendedPictographic = 3537
              },
          graphemeBreakProperties
        }

    graphemeBreakProperties :: GraphemeBreakPropertyAssertions
    graphemeBreakProperties =
      MkGraphemeBreakPropertiesI $
        HMap.fromList
          [ (GraphemeClusterBreak_Prepend, 28),
            (GraphemeClusterBreak_CR, 1),
            (GraphemeClusterBreak_LF, 1),
            (GraphemeClusterBreak_Control, 3893),
            (GraphemeClusterBreak_Extend, 2198),
            (GraphemeClusterBreak_Regional_Indicator, 26),
            (GraphemeClusterBreak_SpacingMark, 378),
            (GraphemeClusterBreak_L, 125),
            (GraphemeClusterBreak_V, 100),
            (GraphemeClusterBreak_T, 137),
            (GraphemeClusterBreak_LV, 399),
            (GraphemeClusterBreak_LVT, 10773),
            (GraphemeClusterBreak_ZWJ, 1)
          ]
