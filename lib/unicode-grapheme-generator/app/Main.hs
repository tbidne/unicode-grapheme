{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import System.OsPath (osp, (</>))
import Unicode.Grapheme.Generator.DB.V14_0 qualified as V14_0
import Unicode.Grapheme.Generator.DB.V15_0 qualified as V15_0
import Unicode.Grapheme.Generator.DB.V15_1 qualified as V15_1
import Unicode.Grapheme.Generator.DB.V16_0 qualified as V16_0

main :: IO ()
main = do
  V14_0.generateModule dataDir Nothing
  V15_0.generateModule dataDir Nothing
  V15_1.generateModule dataDir Nothing
  V16_0.generateModule dataDir Nothing
  where
    dataDir =
      Just $
        [osp|lib|]
          </> [osp|unicode-grapheme-generator|]
          </> [osp|data|]
