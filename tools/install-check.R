#!/usr/bin/env Rscript
# Identity and layout checks for an installed rtauargus.native binary,
# run by the release install jobs in a fresh R library.
#
# Usage:
#   Rscript tools/install-check.R <lib> <expected-engine-sha> \
#       <expected-highs> <expected-version>
#
# Exits non-zero (stop) on any mismatch; logs version, path and the
# native provenance on success.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L)
  stop("usage: install-check.R <lib> <engine-sha> <highs> <version>")
lib <- normalizePath(args[1L], mustWork = TRUE)
expected_sha <- args[2L]
expected_highs <- args[3L]
expected_version <- args[4L]

stopifnot(dir.exists(file.path(lib, "rtauargus.native")))
pkg_path <- find.package("rtauargus.native", lib.loc = lib)
stopifnot(normalizePath(pkg_path) == file.path(lib, "rtauargus.native"))

library(rtauargus.native)

version <- as.character(packageVersion("rtauargus.native"))
cat("package:", pkg_path, "\n")
cat("version:", version, "\n")
if (!identical(version, expected_version))
  stop("package version ", version,
       " does not match the built version ", expected_version)

meta <- tauargus_engine_meta()
cat("engine_sha:", meta[["engine_sha"]], "\n")
cat("highs_version:", meta[["highs_version"]], "\n")
if (!identical(meta[["engine_sha"]], expected_sha))
  stop("installed engine ", meta[["engine_sha"]],
       " does not match the pinned engine ", expected_sha)
if (!identical(meta[["highs_version"]], expected_highs))
  stop("installed HiGHS ", meta[["highs_version"]],
       " does not match the pinned HiGHS ", expected_highs)

# The main native library must be loaded from the installed package.
dlls <- getLoadedDLLs()
loaded <- vapply(dlls, function(d) d[["path"]], character(1))
loaded_names <- vapply(dlls, function(d) d[["name"]], character(1))
main <- loaded[loaded_names == "rtauargus.native"]
if (length(main) != 1L)
  stop("the rtauargus.native shared object is not loaded")
cat("loaded native library:", main, "\n")
# Normalize both sides: on Windows the loaded-DLL path and
# normalizePath may disagree on the separator style.
if (!startsWith(normalizePath(main), normalizePath(pkg_path)))
  stop("native library loaded from outside the installation: ", main)

# The bundled engine runtime libraries must sit inside the installed
# package directory (the layout shipped in 0.5.6).
if (.Platform$OS.type == "windows") {
  lib_dir <- file.path(pkg_path, "libs", "x64")
  expected <- c("rtauargus.native.dll", "libtauargus_csp.dll",
                "libtauargus_hitas.dll", "libtauargus_rounder.dll",
                "libCRP.dll")
} else if (Sys.info()["sysname"] == "Darwin") {
  lib_dir <- file.path(pkg_path, "libs")
  expected <- c("rtauargus.native.so", "libtauargus_csp.dylib",
                "libtauargus_hitas.dylib", "libtauargus_rounder.dylib",
                "libCRP.dylib")
} else {
  lib_dir <- file.path(pkg_path, "libs")
  expected <- c("rtauargus.native.so", "libtauargus_csp.so",
                "libtauargus_hitas.so", "libtauargus_rounder.so",
                "libCRP.so")
}
missing <- expected[!file.exists(file.path(lib_dir, expected))]
if (length(missing) > 0L)
  stop("missing bundled libraries in ", lib_dir, ": ",
       paste(missing, collapse = ", "))

cat("install-check ok:", version, meta[["engine_sha"]],
    meta[["highs_version"]], "\n")
