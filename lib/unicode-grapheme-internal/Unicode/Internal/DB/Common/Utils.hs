{-# LANGUAGE QuasiQuotes #-}

module Unicode.Internal.DB.Common.Utils
  ( -- * Property Index
    PropertiesIndex (..),
    PropertiesF,

    -- * Unicode DB
    mkUnicodePath,
  )
where

import Data.HashSet (HashSet)
import Data.Kind (Type)
import System.OsPath (OsPath, osp, (</>))
import Unicode.Internal.Version (UnicodeVersion)
import Unicode.Internal.Version qualified as Vers

-- | Index for properties. This allows us to use the same type for data
-- and assertions.
data PropertiesIndex
  = PropertiesAssertions
  | PropertiesData

type PropertiesF :: PropertiesIndex -> Type
type family PropertiesF p where
  PropertiesF PropertiesAssertions = Int
  PropertiesF PropertiesData = HashSet Char

-- | Path to some unicode file.
mkUnicodePath :: UnicodeVersion -> OsPath -> OsPath
mkUnicodePath uvers p =
  [osp|data|]
    </> Vers.versToFolderName uvers
    </> p
