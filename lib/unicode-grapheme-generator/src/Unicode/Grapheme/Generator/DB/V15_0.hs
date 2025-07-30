{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.DB.V15_0
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
import Unicode.Grapheme.Common.Version (UnicodeVersion (UnicodeVersion_15_0))
import Unicode.Grapheme.Generator.DB.EmojiData qualified as EmojiData
import Unicode.Grapheme.Generator.DB.GraphemeBreakProperty qualified as GraphemeBreakProperty
import Unicode.Grapheme.Generator.Utils qualified as Utils

generateModule :: Maybe OsPath -> Maybe OsPath -> IO ()
generateModule mDataDir mDestDir = do
  emojiTxt <- EmojiData.generateData mDataDir 3537 vers
  gbpTxt <- GraphemeBreakProperty.generateData mDataDir gbpAsserts vers

  let txt =
        Utils.tunlines
          [ "module " <> Utils.mkModuleHeaderName vers,
            "  ( -- * Emojis",
            "    extendedPictographic,",
            "",
            "    -- * Grapheme Cluster Breaks",
            "    graphemeBreakProperties,",
            "  )",
            "where\n",
            imports,
            emojiTxt,
            gbpTxt
          ]

  Utils.writeModule mDestDir vers txt
  where
    imports =
      T.unlines
        [ "import Data.HashMap.Strict (HashMap)",
          "import Data.HashMap.Strict qualified as HMap",
          "import Data.HashSet (HashSet)",
          "import Data.HashSet qualified as HSet",
          "import Unicode.Grapheme.Common.DB.GraphemeClusterBreak",
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

vers :: UnicodeVersion
vers = UnicodeVersion_15_0
