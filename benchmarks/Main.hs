{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import System.File.OsPath qualified as FileIO
import System.OsPath (osp, (</>))
import Test.Tasty.Bench (Benchmark)
import Test.Tasty.Bench qualified as Bench
import Unicode.Grapheme qualified as Grapheme

main :: IO ()
main = do
  Bench.defaultMain
    [ Bench.env readSampleText breakSample
    ]

breakSample :: Text -> Benchmark
breakSample sample =
  Bench.bgroup
    "Sample"
    (benchVers <$> [minBound .. maxBound])
  where
    benchVers v =
      Bench.bench (Grapheme.displayVersion v) $
        Bench.nf (Grapheme.breakGraphemeClustersVersion v) sample

readSampleText :: IO Text
readSampleText = do
  bs <- FileIO.readFile' path
  pure $ TEnc.decodeUtf8 bs
  where
    path =
      [osp|benchmarks|] </> [osp|sample.txt|]
