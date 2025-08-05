{-# LANGUAGE QuasiQuotes #-}

module Unicode.Grapheme.Generator.Version
  ( -- * Version
    UnicodeVersion (..),

    -- * Version to names
    versToFolderName,
    versToModuleName,

    -- * Display
    displayVersion,
    displayModuleName,
  )
where

import Data.String (IsString)
import System.OsPath (OsPath, osp)

-- NOTE: Should be kept in sync with Unicode.Grapheme.Internal.Version
-- for common functionality (API does not have to be the same).

data UnicodeVersion
  = -- | @since 0.1
    UnicodeVersion_14_0
  | -- | @since 0.1
    UnicodeVersion_15_0
  | -- | @since 0.1
    UnicodeVersion_15_1
  | -- | @since 0.1
    UnicodeVersion_16_0
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Enum,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

versToFolderName :: UnicodeVersion -> OsPath
versToFolderName UnicodeVersion_14_0 = [osp|14_0|]
versToFolderName UnicodeVersion_15_0 = [osp|15_0|]
versToFolderName UnicodeVersion_15_1 = [osp|15_1|]
versToFolderName UnicodeVersion_16_0 = [osp|16_0|]

versToModuleName :: UnicodeVersion -> OsPath
versToModuleName UnicodeVersion_14_0 = [osp|V14_0|]
versToModuleName UnicodeVersion_15_0 = [osp|V15_0|]
versToModuleName UnicodeVersion_15_1 = [osp|V15_1|]
versToModuleName UnicodeVersion_16_0 = [osp|V16_0|]

displayModuleName :: (IsString s) => UnicodeVersion -> s
displayModuleName UnicodeVersion_14_0 = "V14_0"
displayModuleName UnicodeVersion_15_0 = "V15_0"
displayModuleName UnicodeVersion_15_1 = "V15_1"
displayModuleName UnicodeVersion_16_0 = "V16_0"

-- | Textual representation.
--
-- @since 0.1
displayVersion :: (IsString s) => UnicodeVersion -> s
displayVersion UnicodeVersion_14_0 = "14.0"
displayVersion UnicodeVersion_15_0 = "15.0"
displayVersion UnicodeVersion_15_1 = "15.1"
displayVersion UnicodeVersion_16_0 = "16.0"
