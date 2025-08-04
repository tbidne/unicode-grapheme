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
import Unicode.Grapheme.Internal.ClusterState (ClusterState, Rule, RulesMatched)
import Unicode.Grapheme.Internal.ClusterState qualified as ClusterState
import Unicode.Grapheme.Internal.V14_0.DB
  ( UnicodeDatabase (MkUnicodeDatabase),
    database,
  )
import Unicode.Grapheme.Internal.V14_0.DB qualified as V14_0.DB
import Unicode.Grapheme.Internal.V15_0 qualified as V15_0
import Unicode.Grapheme.Internal.V15_0.DB (UnicodeDatabase (MkUnicodeDatabase))
import Unicode.Grapheme.Internal.Width qualified as Width

breakGraphemeClusters :: Text -> [Text]
breakGraphemeClusters =
  ClusterState.breakGraphemeClusters database rules

breakGraphemeClustersRules :: Text -> (RulesMatched, [Text])
breakGraphemeClustersRules =
  ClusterState.breakGraphemeClustersRules database rules

breakGraphemeClustersStates :: Text -> Seq ClusterState
breakGraphemeClustersStates =
  ClusterState.breakGraphemeClustersStates database rules

clusterWidth :: Text -> Int
clusterWidth = Width.clusterWidth database.unUnicodeDatabase

-- NOTE: 14.0 has the same rules as 15.0
--
-- https://www.unicode.org/reports/tr29/tr29-39.html

rules :: [Rule V14_0.DB.UnicodeDatabase]
rules = coerce V15_0.rules
