module Unicode.Internal.V16_0
  ( databaseTH,
    rules,
  )
where

import Data.Coerce (coerce)
import Language.Haskell.TH (Code, Q)
import Unicode.Internal.ClusterState (Rule)
import Unicode.Internal.Utils qualified as Utils
import Unicode.Internal.V15_1 qualified as V15_1
import Unicode.Internal.V15_1.DB (UnicodeDatabase (MkUnicodeDatabase))
import Unicode.Internal.V16_0.DB
  ( UnicodeDatabase (MkUnicodeDatabase),
    mkUnicodeDatabaseIO,
  )
import Unicode.Internal.V16_0.DB qualified as V16_0.DB

databaseTH :: Code Q V16_0.DB.UnicodeDatabase
databaseTH = Utils.liftIOToTH mkUnicodeDatabaseIO

-- NOTE: 16.0 has the same rules as 15.1.
--
-- https://www.unicode.org/reports/tr29/#Extend

rules :: [Rule V16_0.DB.UnicodeDatabase]
rules = coerce V15_1.rules
