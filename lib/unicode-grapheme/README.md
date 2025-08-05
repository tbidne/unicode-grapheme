<div align="center">

# unicode-grapheme

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/unicode-grapheme?include_prereleases&sort=semver)](https://github.com/tbidne/unicode-grapheme/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/unicode-grapheme/ci.yaml?branch=main)](https://github.com/tbidne/unicode-grapheme/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/unicode-grapheme?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

# Description

`unicode-grapheme` is a library for handling grapheme clusters without any external dependencies. In particular, the primary motivation is breaking text into grapheme clusters i.e. providing functions:

```haskell
-- A 'UnicodeFunction a b' is a function (a -> b) that works across all supported unicode versions.
data UnicodeFunction a b

-- Running UnicodeFunction.
runUnicodeFunctionVersion :: UnicodeVersion -> UnicodeFunction a b -> a -> b

-- Provided UnicodeFunctions:

-- 1. Breaking the Text into grapheme clusters.
breakGraphemeClusters :: UnicodeFunction Text [Text]

-- 2. Returns the approximate "width" of the Text i.e. Text.length that
-- is sensitive to grapheme clusters and "wide" characters.
textWidth :: UnicodeFunction Text Int
```

```haskell
-- Usage.
break :: Text -> [Text]
break = runUnicodeFunctionVersion UnicodeVersion_16_0 breakGraphemeClusters
```

# Library comparisons

## Overview

- 🌕: Supported.
- 🌓: Partial support.
- 🌑: Not supported.

| 👇 Feature / Library 👉   | `unicode-grapheme` | `text-icu` | `unicode-data` | `wcwidth` |
|:--------------------------|-------------------:|-----------:|---------------:|----------:|
| Grapheme clusters         |                 🌕 |         🌕 |             🌑 |        🌑 |
| Cluster width             |                 🌕 |         🌓 |             🌑 |        🌕 |
| No external dependencies  |                 🌕 |         🌑 |             🌕 |        🌑 |

- Grapheme clusters: If the library supports breaking text into grapheme clusters out of the box.
- Cluster width: If the library supports determining text width e.g. understands "wide" properties.
- No external dependencies: If the library requires no non-haskell dependencies.

## text-icu

`text-icu` provides bindings to [ICU](https://github.com/unicode-org/icu) i.e. the `libicu`, C++ library. If a dependency on `libicu` is not too onerous, this is probably the most robust and forwards-compatible approach, as ICU is the de-facto standard.

While there does not appear to be anything like `clusterWidth :: Text -> Int`, ICU is comprehensive enough that it is probably possibly to implement yourself, using the provided utilities.

## unicode-data

`unicode-data` provides access to the unicode database, with a focus on performance. It does not currently offer the properties needed to determine grapheme cluster breaks or text width, though this could likely be added.

Additionally, `unicode-data` works for a single unicode version at a time.

## wcwidth

`wcwidth` provides bindings to the system `wcwidth` function, which attempts to measure the width of a grapheme cluster. It does not provide breaking text into clusters, however.
