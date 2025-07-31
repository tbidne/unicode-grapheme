{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import System.Directory.OsPath qualified as Dir
import System.OsPath (OsPath, osp, (</>))
import Test.Tasty.Bench (Benchmark)
import Test.Tasty.Bench qualified as Bench
import Unicode.Grapheme.Common.Version qualified as Version
import Unicode.Grapheme.Generator.DB.V15_0 qualified as V15_0
import Unicode.Grapheme.Generator.DB.V15_1 qualified as V15_1
import Unicode.Grapheme.Generator.DB.V16_0 qualified as V16_0

main :: IO ()
main = do
  bracket setup teardown $ \tmp ->
    Bench.defaultMain
      [ benchGenerators tmp
      ]

benchGenerators :: OsPath -> Benchmark
benchGenerators destDir =
  Bench.bgroup
    "generateModule"
    [ Bench.bench "15.0" $ Bench.nfIO v15_0,
      Bench.bench "15.1" $ Bench.nfIO v15_1,
      Bench.bench "16.0" $ Bench.nfIO v16_0
    ]
  where
    v15_0 = V15_0.generateModule Nothing (Just destDir)
    v15_1 = V15_1.generateModule Nothing (Just destDir)
    v16_0 = V16_0.generateModule Nothing (Just destDir)

setup :: IO OsPath
setup = do
  tmp <- mkTmp <$> Dir.getTemporaryDirectory
  for_ subDirs $ \d -> Dir.createDirectoryIfMissing True (tmp </> d)
  pure tmp
  where
    mkTmp d = d </> [osp|unicode-grapheme-generator|]

    subDirs :: [OsPath]
    subDirs =
      [minBound .. maxBound] <&> \v ->
        Version.versToModuleName v </> [osp|DB|]

teardown :: OsPath -> IO ()
teardown = Dir.removeDirectoryRecursive
