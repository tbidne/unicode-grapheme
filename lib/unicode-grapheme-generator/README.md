<div align="center">

# unicode-grapheme-generator

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/unicode-grapheme?include_prereleases&sort=semver)](https://github.com/tbidne/unicode-grapheme/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/unicode-grapheme/ci.yaml?branch=main)](https://github.com/tbidne/unicode-grapheme/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/unicode-grapheme?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

# Description

This package provides an executable `unicode-grapheme-generator` that generates modules for `unicode-grapheme` based on the unicode database. That is:

```sh
$ cabal run unicode-grapheme-generator
```

Will create modules `lib/unicode-grapheme/unicode-grapheme-internal/Unicode/Grapheme/Internal/<version>/DB/Generated.hs` that provide definitions like:

```haskell
module Unicode.Grapheme.Internal.V15_0.DB.Generated
  ( derivedEastAsianWide,
    graphemeBreakProperties,
    ...
  )
where

derivedEastAsianWide :: [(Char, Maybe Char)]
derivedEastAsianWide =
  [ ('\x3000', Nothing),
    ('\xFF01', Just '\xFF03'),
  ...
  ]

graphemeBreakProperties :: [(Char, Maybe Char, GraphemeClusterBreak)]
graphemeBreakProperties =
  [ ('\x0600', Just '\x0605', GraphemeClusterBreak_Prepend),
    ('\x06DD', Nothing, GraphemeClusterBreak_Prepend),
  ...
  ]
```

These modules are generated based on unicode database files (e.g. `GraphemeBreakProperty.txt`).
