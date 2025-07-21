{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unicode.Internal.Version
  ( UnicodeVersion (..),
    versToFolderName,
    getBaseVersionTH,
    getBaseVersionIO,
    getBaseVersion,
    displayVersion,
  )
where

import Control.Exception (Exception (displayException), throwIO)
import Data.List qualified as L
import Data.Version (Version (Version), showVersion)
import GHC.Unicode qualified
import Language.Haskell.TH.Syntax (Code, Lift, Q)
import System.OsPath (OsPath, osp)
import Unicode.Internal.Utils qualified as Utils

-- NOTE:
--
-- GHC  | Base | Unicode
-- ---------------------
-- 9.12 | 4.21 | 16.0.0
-- 9.10 | 4.20 | 15.1.0
-- 9.8  | 4.19 | 15.1.0
-- 9.6  | 4.18 | 15.0.0
-- 9.4  | 4.17 | 14.0.0
-- 9.2  | 4.16 | 14.0.0

data UnicodeVersion
  = UnicodeVersion_15_0
  | UnicodeVersion_15_1
  | UnicodeVersion_16_0
  deriving stock (Bounded, Enum, Eq, Lift, Show)

versToFolderName :: UnicodeVersion -> OsPath
versToFolderName UnicodeVersion_15_0 = [osp|15_0|]
versToFolderName UnicodeVersion_15_1 = [osp|15_1|]
versToFolderName UnicodeVersion_16_0 = [osp|16_0|]

displayVersion :: UnicodeVersion -> String
displayVersion UnicodeVersion_15_0 = "15.0"
displayVersion UnicodeVersion_15_1 = "15.1"
displayVersion UnicodeVersion_16_0 = "16.0"

allVersString :: String
allVersString =
  L.intercalate ", " $
    displayVersion <$> [minBound :: UnicodeVersion .. maxBound]

getBaseVersionTH :: Code Q UnicodeVersion
getBaseVersionTH = Utils.liftIOToTH getBaseVersionIO

getBaseVersionIO :: IO UnicodeVersion
getBaseVersionIO = case getBaseVersion of
  Right vers -> pure vers
  Left ex -> throwIO ex

getBaseVersion :: Either UnsupportedUnicodeE UnicodeVersion
getBaseVersion = case vers of
  [15, 0, 0] -> Right UnicodeVersion_15_0
  [15, 1, 0] -> Right UnicodeVersion_15_1
  [16, 0, 0] -> Right UnicodeVersion_16_0
  _ -> Left $ MkUnsupportedUnicodeE version
  where
    version@(Version vers _) = GHC.Unicode.unicodeVersion

newtype UnsupportedUnicodeE = MkUnsupportedUnicodeE Version
  deriving stock (Eq, Show)

instance Exception UnsupportedUnicodeE where
  displayException (MkUnsupportedUnicodeE vers) =
    mconcat
      [ "Unsupported unicode version '",
        showVersion vers,
        "'. Supported versions are: ",
        allVersString
      ]
