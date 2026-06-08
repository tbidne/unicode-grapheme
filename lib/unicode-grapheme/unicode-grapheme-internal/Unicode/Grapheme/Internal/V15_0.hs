module Unicode.Grapheme.Internal.V15_0
  ( -- * Breakers
    breakGraphemeClusters,
    breakGraphemeClustersRules,
    breakGraphemeClustersStates,

    -- * Width
    clusterWidth,

    -- * Rules
    rules,
  )
where

import Data.Coerce (coerce)
import Data.Sequence (Seq)
import Data.Text (Text)
import Unicode.Grapheme.Internal.ClusterState (ClusterState, Rule, RuleMatched)
import Unicode.Grapheme.Internal.ClusterState qualified as ClusterState
import Unicode.Grapheme.Internal.V15_0.DB
  ( UnicodeDatabase (MkUnicodeDatabase),
    database,
  )
import Unicode.Grapheme.Internal.V15_0.DB qualified as V15_0.DB
import Unicode.Grapheme.Internal.V15_1 qualified as V15_1
import Unicode.Grapheme.Internal.V15_1.DB (UnicodeDatabase (MkUnicodeDatabase))
import Unicode.Grapheme.Internal.Width qualified as Width

-- | Breaks 'Text' into grapheme clusters.
--
-- ==== __Examples__
--
-- >>> breakGraphemeClusters "abc"
-- ["a","b","c"]
--
-- >>> -- U+004F U+0308
-- >>> breakGraphemeClusters "Ö"
-- ["O\776"]
--
-- >>> -- 🧑‍🌾
-- >>> breakGraphemeClusters "\x1F9D1\x200D\x1F33E"
-- ["\129489\8205\127806"]
--
-- @since 0.1
breakGraphemeClusters :: Text -> [Text]
breakGraphemeClusters =
  ClusterState.breakGraphemeClusters database rules

breakGraphemeClustersRules :: Text -> (Seq RuleMatched, [Text])
breakGraphemeClustersRules =
  ClusterState.breakGraphemeClustersRules database rules

breakGraphemeClustersStates :: Text -> Seq ClusterState
breakGraphemeClustersStates =
  ClusterState.breakGraphemeClustersStates database rules

-- | Given a __single__ grapheme cluster -- of possibly multiple code points --
-- returns the width 1 or 2. This is based on heuristics i.e. if the text
-- contains at least one code point with the following properties:
--
--    - East_Asian_Width = Fullwidth or Wide
--    - Emoji_Presentation
--    - U+FE0F (emoji-style)
--
-- Then width is 2. Otherwise it is 1.
--
-- ===== __Examples__
--
--
-- >>> clusterWidth "a"
-- 1
--
-- >>> clusterWidth "🇯🇵"
-- 2
--
-- >>> -- Used with multiple clusters can lead to unexpected results!
-- >>> clusterWidth "abc"
-- 1
--
-- @since 0.1
clusterWidth :: Text -> Int
clusterWidth = Width.clusterWidth database.unUnicodeDatabase

-- NOTE: 15.0 has the same rules as 15.1 except not GB9c.
--
-- https://www.unicode.org/reports/tr29/tr29-41.html
--
-- See NOTE: [Unicode rules]

rules :: [Rule V15_0.DB.UnicodeDatabase]
rules =
  coerce
    [ V15_1.gb1,
      V15_1.gb2,
      V15_1.gb3,
      V15_1.gb4,
      V15_1.gb5,
      V15_1.gb6,
      V15_1.gb7,
      V15_1.gb8,
      V15_1.gb9,
      V15_1.gb9a,
      V15_1.gb9b,
      V15_1.gb11,
      V15_1.gb12_13
    ]
