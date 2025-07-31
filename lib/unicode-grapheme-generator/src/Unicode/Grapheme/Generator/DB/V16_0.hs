{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.V16_0
  ( generateModule,
  )
where

import Data.HashMap.Strict qualified as HMap
import Data.Text qualified as T
import System.OsPath (OsPath)
import Unicode.Grapheme.Common.DB.GraphemeClusterBreak
  ( GraphemeClusterBreak
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
import Unicode.Grapheme.Common.Version (UnicodeVersion (UnicodeVersion_16_0))
import Unicode.Grapheme.Generator.DB.DerivedCoreProperty qualified as DerivedCoreProperty
import Unicode.Grapheme.Generator.DB.EmojiData qualified as EmojiData
import Unicode.Grapheme.Generator.DB.GraphemeBreakProperty qualified as GraphemeBreakProperty
import Unicode.Grapheme.Generator.Utils qualified as Utils

generateModule :: Maybe OsPath -> Maybe OsPath -> IO ()
generateModule mDataDir mDestDir = do
  derivedTxt <- DerivedCoreProperty.generateData mDataDir (240, 2192, 6) vers
  emojiTxt <- EmojiData.generateData mDataDir 3537 vers
  gbpTxt <- GraphemeBreakProperty.generateData mDataDir gbpAsserts vers

  let txt =
        Utils.tunlines
          [ "module " <> Utils.mkModuleHeaderName vers,
            "  ( -- * Derived Core Properties",
            "    derivedCore_IndicConjunctBreak_Consonant,",
            "    derivedCore_IndicConjunctBreak_Extend,",
            "    derivedCore_IndicConjunctBreak_Linker,",
            "",
            "    -- * Emojis",
            "    extendedPictographic,",
            "",
            "    -- * Grapheme Cluster Breaks",
            "    graphemeBreakProperties,",
            "  )",
            "where\n",
            imports,
            derivedTxt,
            emojiTxt,
            gbpTxt
          ]

  Utils.writeModule mDestDir vers txt
  where
    imports =
      T.unlines
        [ "import Unicode.Grapheme.Common.DB.GraphemeClusterBreak",
          "  ( GraphemeClusterBreak",
          "      ( GraphemeClusterBreak_CR,",
          "        GraphemeClusterBreak_Control,",
          "        GraphemeClusterBreak_Extend,",
          "        GraphemeClusterBreak_L,",
          "        GraphemeClusterBreak_LF,",
          "        GraphemeClusterBreak_LV,",
          "        GraphemeClusterBreak_LVT,",
          "        GraphemeClusterBreak_Prepend,",
          "        GraphemeClusterBreak_Regional_Indicator,",
          "        GraphemeClusterBreak_SpacingMark,",
          "        GraphemeClusterBreak_T,",
          "        GraphemeClusterBreak_V,",
          "        GraphemeClusterBreak_ZWJ",
          "      ),",
          "  )"
        ]

    gbpAsserts =
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

vers :: UnicodeVersion
vers = UnicodeVersion_16_0
