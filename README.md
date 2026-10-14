# rtauargus.native

An R interface for [Tau-Argus](https://www.sdcweb.org/tau-argus/):
statistical disclosure control for tables and microdata.

This is a fork of INSEE's
[rtauargus](https://github.com/inseefrlab/rtauargus). Upstream runs the
legacy Tau-Argus executable; this fork executes the same `.arb` batches in
process through the native, HiGHS-backed
[tauargus-engine](https://github.com/trygu/tauargus-engine). Function
names, arguments and return values follow upstream; see
[docs/compatibility.md](docs/compatibility.md) for the intentional
differences. The package is self-contained: the native engine and HiGHS
are bundled in it and build at install time, so there is one package to
install, not two.

## Ownership and dependencies

- This repo owns the R front-end: package, tests, releases.
- The native engine and the R binding live in
  [tauargus-engine](https://github.com/trygu/tauargus-engine) and are
  pinned here as a git submodule (`tauargus-engine/`; the gitlink is the
  pin). The release pipeline stages the native layer into this package
  from that pinned source; no separately published engine release is
  needed.

## Installation

Each GitHub Release (tags `r-v*`) contains one binary per platform plus a
source tarball. The binary installs without a compiler; the source
tarball builds the bundled engine at install time and needs CMake >= 3.16
and a C++11 compiler.

Linux:

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.4/rtauargus.native_1.3.7_r4.5.3_linux-x86_64.tar.gz",
  repos = NULL, type = "binary")
library(rtauargus.native)
```

macOS (Apple silicon; the release targets arm64):

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.4/rtauargus.native_1.3.7_r4.5.3_macos-arm64.tar.gz",
  repos = NULL, type = "binary")
library(rtauargus.native)
```

Windows (the release file is a `.zip`):

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.4/rtauargus.native_1.3.7_r4.5.3_windows-x86_64.zip",
  repos = NULL, type = "binary")
library(rtauargus.native)
```

Replace `r-v0.5.4` with the latest release tag. The binaries are built
for R 4.5.3 (the `r4.5.3` in the file name); other R versions can use the
source tarball:

```r
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.4/rtauargus.native_1.3.7.tar.gz",
  repos = NULL)
```

The CRAN dependencies (dplyr, igraph, sdcHierarchies, ...) install from
CRAN as usual.

## Quick test

A smoke test that the engine built, links and loads:

```r
library(rtauargus.native)
engine <- new_tauargus()
tauargus_version(engine)       # engine version string
stopifnot(!is.null(new_hitas()), !is.null(new_rounder()))
tauargus_engine_meta()         # engine SHA + HiGHS version of this build
```

`tauargus_engine_meta()` reports the exact engine commit and HiGHS
version the installed package was built from. The native audit (CSP
intervalle check) is exposed as `audit_jj(jjfile)`; the front-end uses it
through the engine object's `audit(tab)` method and the `with_audit`
table export.

## Development

Bump the `tauargus-engine` submodule to the engine commit you want, then
push. The pipeline (`.github/workflows/release.yml`) stages the native
layer into this package from the submodule, builds the source tarball,
installs it in a fresh library (the package's configure builds the
bundled engine) and runs the full test suite on Linux, macOS and Windows;
on `r-v*` tags it also runs a clean-room install of the release
artifacts and cuts the GitHub Release.
