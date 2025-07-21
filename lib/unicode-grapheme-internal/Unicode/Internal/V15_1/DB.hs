{-# LANGUAGE TemplateHaskell #-}

module Unicode.Internal.V15_1.DB
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
import Unicode.Internal.DB.Common.Utils (PropertiesIndex (PropertiesAssertions))
import Unicode.Internal.Version (UnicodeVersion (UnicodeVersion_15_1))

newtype UnicodeDatabase = MkUnicodeDatabase
  { unUnicodeDatabase :: Properties
  }
  deriving stock (Eq, Lift, Show)

mkUnicodeDatabaseIO :: IO UnicodeDatabase
mkUnicodeDatabaseIO =
  MkUnicodeDatabase <$> readUnicodeDataIO asserts UnicodeVersion_15_1
  where
    asserts :: PropertiesI PropertiesAssertions
    asserts =
      MkPropertiesI
        { derivedCoreProperties =
            MkDerivedCorePropertiesI
              { indicConjunctBreakConsonant = 240,
                indicConjunctBreakExtend = 884,
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
          [ (GraphemeClusterBreak_Prepend, 27),
            (GraphemeClusterBreak_CR, 1),
            (GraphemeClusterBreak_LF, 1),
            (GraphemeClusterBreak_Control, 3893),
            (GraphemeClusterBreak_Extend, 2130),
            (GraphemeClusterBreak_Regional_Indicator, 26),
            (GraphemeClusterBreak_SpacingMark, 395),
            (GraphemeClusterBreak_L, 125),
            (GraphemeClusterBreak_V, 95),
            (GraphemeClusterBreak_T, 137),
            (GraphemeClusterBreak_LV, 399),
            (GraphemeClusterBreak_LVT, 10773),
            (GraphemeClusterBreak_ZWJ, 1)
          ]
