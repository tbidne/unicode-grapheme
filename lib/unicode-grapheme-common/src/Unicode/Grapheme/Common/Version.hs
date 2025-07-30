{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Common.Version
  ( UnicodeVersion (..),
    versToFolderName,
    versToModuleName,
    getBaseVersionIO,
    getBaseVersion,
    displayVersion,
    displayModuleName,
  )
where

import Control.Exception (Exception (displayException), throwIO)
import Data.List qualified as L
import Data.String (IsString)
import Data.Version (Version (Version), showVersion)
import GHC.Unicode qualified
import Language.Haskell.TH.Syntax (Lift)
import System.OsPath (OsPath, osp)

data UnicodeVersion
  = UnicodeVersion_15_0
  | UnicodeVersion_15_1
  | UnicodeVersion_16_0
  deriving stock (Bounded, Enum, Eq, Lift, Show)

versToFolderName :: UnicodeVersion -> OsPath
versToFolderName UnicodeVersion_15_0 = [osp|15_0|]
versToFolderName UnicodeVersion_15_1 = [osp|15_1|]
versToFolderName UnicodeVersion_16_0 = [osp|16_0|]

versToModuleName :: UnicodeVersion -> OsPath
versToModuleName UnicodeVersion_15_0 = [osp|V15_0|]
versToModuleName UnicodeVersion_15_1 = [osp|V15_1|]
versToModuleName UnicodeVersion_16_0 = [osp|V16_0|]

displayModuleName :: (IsString s) => UnicodeVersion -> s
displayModuleName UnicodeVersion_15_0 = "V15_0"
displayModuleName UnicodeVersion_15_1 = "V15_1"
displayModuleName UnicodeVersion_16_0 = "V16_0"

displayVersion :: (IsString s) => UnicodeVersion -> s
displayVersion UnicodeVersion_15_0 = "15.0"
displayVersion UnicodeVersion_15_1 = "15.1"
displayVersion UnicodeVersion_16_0 = "16.0"

allVersString :: String
allVersString =
  L.intercalate ", " $
    displayVersion <$> [minBound :: UnicodeVersion .. maxBound]

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
