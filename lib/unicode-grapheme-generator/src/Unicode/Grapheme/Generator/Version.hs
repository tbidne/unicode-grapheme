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

import Data.String (IsString, fromString)
import System.OsPath (OsPath, osp)
import System.OsString qualified as OsStr

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
  | -- | @since 0.1
    UnicodeVersion_17_0
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

-- |
--
-- >>> versToFolderName UnicodeVersion_17_0
-- "17_0"
versToFolderName :: UnicodeVersion -> OsPath
versToFolderName UnicodeVersion_14_0 = [osp|14_0|]
versToFolderName UnicodeVersion_15_0 = [osp|15_0|]
versToFolderName UnicodeVersion_15_1 = [osp|15_1|]
versToFolderName UnicodeVersion_16_0 = [osp|16_0|]
versToFolderName UnicodeVersion_17_0 = [osp|17_0|]

-- |
--
-- >>> versToModuleName UnicodeVersion_17_0
-- "V17_0"
versToModuleName :: UnicodeVersion -> OsPath
versToModuleName = OsStr.cons (OsStr.unsafeFromChar 'V') . versToFolderName

-- |
--
-- >>> displayModuleName @String UnicodeVersion_17_0
-- "V17_0"
displayModuleName :: (IsString s) => UnicodeVersion -> s
displayModuleName = fromString . unsafeDecode . versToModuleName

-- | Textual representation.
--
-- >>> displayVersion @String UnicodeVersion_17_0
-- "17.0"
--
-- @since 0.1
displayVersion :: (IsString s) => UnicodeVersion -> s
displayVersion = fromString . unsafeDecode . OsStr.map replaceUs . versToFolderName
  where
    replaceUs c
      | c == us = dot
      | otherwise = c

    us = OsStr.unsafeFromChar '_'
    dot = OsStr.unsafeFromChar '.'

unsafeDecode :: OsPath -> String
unsafeDecode p = case OsStr.decodeUtf p of
  Nothing ->
    error $
      mconcat
        [ "Unicode.Grapheme.Generator.Version.unsafeDecode: ",
          "Could not decode path: ",
          show p
        ]
  Just s -> s
