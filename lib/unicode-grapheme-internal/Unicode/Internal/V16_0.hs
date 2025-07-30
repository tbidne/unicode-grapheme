module Unicode.Internal.V16_0
  ( breakGraphemeClusters,
    breakGraphemeClustersRules,
    breakGraphemeClustersStates,
    database,
    rules,
  )
where

import Data.Coerce (coerce)
import Data.Sequence (Seq)
import Data.Text (Text)
import Unicode.Internal.ClusterState (ClusterState, Rule, RulesMatched)
import Unicode.Internal.ClusterState qualified as ClusterState
import Unicode.Internal.V15_1 qualified as V15_1
import Unicode.Internal.V15_1.DB (UnicodeDatabase (MkUnicodeDatabase))
import Unicode.Internal.V16_0.DB
  ( UnicodeDatabase (MkUnicodeDatabase),
    database,
  )
import Unicode.Internal.V16_0.DB qualified as V16_0.DB

breakGraphemeClusters :: Text -> [Text]
breakGraphemeClusters =
  ClusterState.breakGraphemeClusters database rules

breakGraphemeClustersRules :: Text -> (RulesMatched, [Text])
breakGraphemeClustersRules =
  ClusterState.breakGraphemeClustersRules database rules

breakGraphemeClustersStates :: Text -> Seq ClusterState
breakGraphemeClustersStates =
  ClusterState.breakGraphemeClustersStates database rules

-- NOTE: 16.0 has the same rules as 15.1.
--
-- https://www.unicode.org/reports/tr29/#Extend

rules :: [Rule V16_0.DB.UnicodeDatabase]
rules = coerce V15_1.rules
