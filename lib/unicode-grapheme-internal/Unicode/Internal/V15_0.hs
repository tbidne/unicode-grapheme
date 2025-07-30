module Unicode.Internal.V15_0
  ( database,
    rules,
  )
where

import Data.Coerce (coerce)
import Unicode.Internal.ClusterState (Rule)
import Unicode.Internal.V15_0.DB
  ( UnicodeDatabase (MkUnicodeDatabase),
    database,
  )
import Unicode.Internal.V15_0.DB qualified as V15_0.DB
import Unicode.Internal.V15_1 qualified as V15_1
import Unicode.Internal.V15_1.DB (UnicodeDatabase (MkUnicodeDatabase))

-- NOTE: 15.0 has the same rules as 15.1 except not GB9c.
--
-- https://www.unicode.org/reports/tr29/tr29-40.html

rules :: [Rule V15_0.DB.UnicodeDatabase]
rules =
  coerce
    [ V15_1.gb2,
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
