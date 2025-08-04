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
-- each element is one grapheme cluster
breakGraphemeClusters :: UnicodeFunction Text [Text]

-- returns the approximate "width" of the text
textWidth :: UnicodeFunction Text Int
```

A `UnicodeFunction a b` is a function `a -> b` that works across multiple unicode versions. For example, to break text for a specific version, we can define:

```haskell
-- runUnicodeFunctionVersion :: UnicodeVersion -> UnicodeFunction a b -> a -> b
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

## wcwidth

`wcwidth` provides bindings to the system `wcwidth` function, which attempts to measure the width of a grapheme cluster. It does not provide breaking text into clusters, however.
