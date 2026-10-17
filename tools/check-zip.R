#!/usr/bin/env Rscript
# Verify that the Windows binary archive is a real zip that contains
# the installed package.
#
# GNU tar (the default on the Windows runner) cannot write zip files:
# `tar -a -c -f x.zip` silently emits a plain tar with a .zip name,
# which R CMD INSTALL then fails to read (the 0.5.6 Windows binary
# was shipped in exactly this state). R's own zip reader is the
# same code path the installer uses, so if it can list the archive,
# the install can read it too.
#
# The second argument is the top-level directory R's unpackPkgZip
# expects to find: the zip file name minus .zip and the trailing
# _[0-9.-]+. The archive must contain <dir>/DESCRIPTION.
#
# Usage:
#   Rscript tools/check-zip.R <archive.zip> <top-level-dir>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L)
  stop("usage: check-zip.R <archive.zip> <top-level-dir>")
z <- utils::unzip(args[1L], list = TRUE)
want <- paste0(args[2L], "/DESCRIPTION")
if (!any(z[["Name"]] == want))
  stop("zip does not contain ", want)
cat("zip ok:", nrow(z), "entries, top-level dir", args[2L], "\n")
