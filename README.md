<div align="center">

# unicode-grapheme

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/unicode-grapheme?include_prereleases&sort=semver)](https://github.com/tbidne/unicode-grapheme/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/unicode-grapheme/ci.yaml?branch=main)](https://github.com/tbidne/unicode-grapheme/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/unicode-grapheme?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

# Base/unicode matrix

The following table lists supported GHC/base, along with `base`'s version of unicode. Note that `base` being supported does **not** imply that its built-in unicode version is supported. For instance, `unicode-grapheme` can be used with base `4.16`, though the user has to explicitly select one of the supported versions of unicode. Its version, `14.0`, is not supported. Such support is listed in the `Base Supported` column.

| Base |  GHC | Unicode | Base Supported |
|-----:|-----:|--------:|---------------:|
| 4.21 | 9.12 |    16.0 |             🌕 |
| 4.20 | 9.10 |    15.1 |             🌕 |
| 4.19 |  9.8 |    15.1 |             🌕 |
| 4.18 |  9.6 |    15.0 |             🌕 |
| 4.17 |  9.4 |    14.0 |             🌑 |
| 4.16 |  9.2 |    14.0 |             🌑 |
