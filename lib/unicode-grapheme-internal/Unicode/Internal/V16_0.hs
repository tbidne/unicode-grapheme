module Unicode.Internal.V16_0
  ( database,
    rules,
  )
where

import Data.Coerce (coerce)
import Unicode.Internal.ClusterState (Rule)
import Unicode.Internal.V15_1 qualified as V15_1
import Unicode.Internal.V15_1.DB (UnicodeDatabase (MkUnicodeDatabase))
import Unicode.Internal.V16_0.DB
  ( UnicodeDatabase (MkUnicodeDatabase),
    database,
  )
import Unicode.Internal.V16_0.DB qualified as V16_0.DB

-- NOTE: 16.0 has the same rules as 15.1.
--
-- https://www.unicode.org/reports/tr29/#Extend

rules :: [Rule V16_0.DB.UnicodeDatabase]
rules = coerce V15_1.rules
