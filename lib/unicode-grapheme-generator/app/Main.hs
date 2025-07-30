module Main (main) where

import Unicode.Grapheme.Generator.DB.V15_0 qualified as V15_0
import Unicode.Grapheme.Generator.DB.V15_1 qualified as V15_1
import Unicode.Grapheme.Generator.DB.V16_0 qualified as V16_0

main :: IO ()
main = do
  V15_0.generateModule Nothing Nothing
  V15_1.generateModule Nothing Nothing
  V16_0.generateModule Nothing Nothing
