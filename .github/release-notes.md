R release: one self-contained `rtauargus.native` package per
platform. The native tauargus-engine binding is now bundled
inside the package (built from the pinned tauargus-engine
submodule); the separate `tauargusengine` package is no
longer released. `rtauargus.native::tauargus_engine_meta()`
reports the exact engine SHA and HiGHS version this build
came from.

Install from this release (prebuilt, no compiler; R @RVERSION@):

Windows:

    install.packages(
      "https://github.com/trygu/rtauargus.native/releases/download/@TAG@/rtauargus.native_@RVER@_r@RVERSION@_windows-x86_64.zip",
      repos = NULL, type = "binary")
    library(rtauargus.native)

macOS (Apple silicon):

    install.packages(
      "https://github.com/trygu/rtauargus.native/releases/download/@TAG@/rtauargus.native_@RVER@_r@RVERSION@_macos-arm64.tar.gz",
      repos = NULL, type = "binary")
    library(rtauargus.native)

Linux:

    install.packages(
      "https://github.com/trygu/rtauargus.native/releases/download/@TAG@/rtauargus.native_@RVER@_r@RVERSION@_linux-x86_64.tar.gz",
      repos = NULL)
    library(rtauargus.native)

`type = "binary"` is only supported on Windows and macOS. On
Linux the prebuilt tarball installs as a source package, so the
argument is omitted.

The CRAN dependencies install from CRAN as usual (on Linux they
compile from source):

    install.packages(c("purrr", "dplyr", "tidyr", "data.table",
                       "gdata", "stringr", "rlang", "zoo",
                       "sdcHierarchies", "igraph", "lifecycle"))

The source tarball builds HiGHS and the engine at install
time (CMake >= 3.16 and a C++11 compiler required) and
installs on any platform.
