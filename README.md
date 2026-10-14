# rtauargus.native

An R interface for [Tau-Argus](https://www.sdcweb.org/tau-argus/):
statistical disclosure control for tables and microdata.

This is a fork of INSEE's
[rtauargus](https://github.com/inseefrlab/rtauargus). Upstream runs the
legacy Tau-Argus executable; this fork executes the same `.arb` batches in
process through the native, HiGHS-backed
[tauargus-engine](https://github.com/trygu/tauargus-engine) via the
`tauargusengine` R binding. Function names, arguments and return values
follow upstream; see [docs/compatibility.md](docs/compatibility.md) for the
intentional differences.

## Ownership and dependencies

- This repo owns the R front-end: package, tests, releases.
- The native engine and the `tauargusengine` binding live in
  [tauargus-engine](https://github.com/trygu/tauargus-engine) and are
  pinned here as a git submodule (`tauargus-engine/`; the gitlink is the
  pin). The release pipeline builds both R packages from that pinned
  source; no separately published engine release is needed.

## Installation

Install both packages from the latest GitHub Release (tags `r-v*`). The
release set contains the `tauargusengine` binary for your platform and the
`rtauargus.native` source tarball (pure R, any platform):

```r
# replace r4.5 and macos-arm64 with your R version and platform
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.3/tauargusengine_0.5.3_r4.5_macos-arm64.tar.gz",
  repos = NULL, type = "binary")
install.packages(
  "https://github.com/trygu/rtauargus.native/releases/download/r-v0.5.3/rtauargus.native_1.3.6.tar.gz",
  repos = NULL)
library(rtauargus.native)
```

Windows installs the `.zip` from the same release. The installed binding
reports its provenance: `tauargusengine::tauargus_engine_meta()`.

## Development

Bump the `tauargus-engine` submodule to the engine commit you want, then
push. The pipeline (`.github/workflows/release.yml`) stages and builds
`tauargusengine` from the submodule, installs both packages in a fresh
library, and runs the full test suite on Linux, macOS and Windows; on
`r-v*` tags it also runs a clean-room install of the release artifacts and
cuts the GitHub Release.
