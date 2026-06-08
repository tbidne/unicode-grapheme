module Unicode.Grapheme.Internal.V14_0
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
import Unicode.Grapheme.Internal.V14_0.DB
  ( UnicodeDatabase (MkUnicodeDatabase),
    database,
  )
import Unicode.Grapheme.Internal.V14_0.DB qualified as V14_0.DB
import Unicode.Grapheme.Internal.V15_0 qualified as V15_0
import Unicode.Grapheme.Internal.V15_0.DB (UnicodeDatabase (MkUnicodeDatabase))
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

-- NOTE: 14.0 has the same rules as 15.0
--
-- https://www.unicode.org/reports/tr29/tr29-39.html
--
-- See NOTE: [Unicode rules]

rules :: [Rule V14_0.DB.UnicodeDatabase]
rules = coerce V15_0.rules
