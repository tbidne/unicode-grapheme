<div align="center">

# unicode-grapheme

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/unicode-grapheme?include_prereleases&sort=semver)](https://github.com/tbidne/unicode-grapheme/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/unicode-grapheme/ci.yaml?branch=main)](https://github.com/tbidne/unicode-grapheme/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/unicode-grapheme?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

# Description

This repository holds several packages related to handling unicode grapheme clusters. See the [primary readme](#./lib/unicode-grapheme/README.md) for more.

The following diagram explains the dependencies, where a solid line indicates a direct library dependency, and a dotted line indicates some other type of dependency.

```mermaid
graph TD;
    A[unicode-grapheme-common] ---> B[unicode-grapheme-internal];
    A --> C{{unicode-grapheme-generator}};
    C -.-> B;
    B --> D[unicode-grapheme];
```
