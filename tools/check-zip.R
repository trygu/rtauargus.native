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
# Usage:
#   Rscript tools/check-zip.R <archive.zip>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L)
  stop("usage: check-zip.R <archive.zip>")
z <- utils::unzip(args[1L], list = TRUE)
if (!any(z[["Name"]] == "rtauargus.native/DESCRIPTION"))
  stop("zip does not contain rtauargus.native/DESCRIPTION")
cat("zip ok:", nrow(z), "entries\n")
