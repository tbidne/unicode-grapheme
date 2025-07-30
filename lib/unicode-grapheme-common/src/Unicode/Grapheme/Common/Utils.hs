{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Common.Utils
  ( mkUnicodePath,
  )
where

import Data.Maybe (fromMaybe)
import System.OsPath (OsPath, osp, (</>))
import Unicode.Grapheme.Common.Version (UnicodeVersion)
import Unicode.Grapheme.Common.Version qualified as Vers

mkUnicodePath :: Maybe OsPath -> UnicodeVersion -> OsPath -> OsPath
mkUnicodePath mdir uvers p =
  dir
    </> Vers.versToFolderName uvers
    </> p
  where
    dir = fromMaybe [osp|data|] mdir
