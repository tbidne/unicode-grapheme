{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import System.File.OsPath qualified as FileIO
import System.OsPath (osp, (</>))
import Test.Tasty.Bench (Benchmark)
import Test.Tasty.Bench qualified as Bench
import Unicode.Grapheme.V14_0 qualified as V14_0
import Unicode.Grapheme.V15_0 qualified as V15_0
import Unicode.Grapheme.V15_1 qualified as V15_1
import Unicode.Grapheme.V16_0 qualified as V16_0
import Unicode.Grapheme.V17_0 qualified as V17_0

main :: IO ()
main = do
  Bench.defaultMain
    [ Bench.env readSampleText breakSample
    ]

breakSample :: Text -> Benchmark
breakSample sample =
  Bench.bgroup
    "Sample"
    (benchVers <$> fns)
  where
    benchVers (v, breakFn) =
      Bench.bench v $ Bench.nf breakFn sample

    fns =
      [ ("14.0", V14_0.breakGraphemeClusters),
        ("15.0", V15_0.breakGraphemeClusters),
        ("15.1", V15_1.breakGraphemeClusters),
        ("16.0", V16_0.breakGraphemeClusters),
        ("17.0", V17_0.breakGraphemeClusters)
      ]

readSampleText :: IO Text
readSampleText = do
  bs <- FileIO.readFile' path
  pure $ TEnc.decodeUtf8 bs
  where
    path =
      [osp|benchmarks|] </> [osp|sample.txt|]
