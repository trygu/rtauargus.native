# Provenance check: the installed binding must report the engine commit
# and the HiGHS version it was built from.

library(tauargusengine)

meta <- tauargus_engine_meta()
stopifnot(!is.na(meta[["engine_sha"]]),
          nchar(meta[["highs_version"]]) > 0L)
cat("engine meta:", meta[["engine_sha"]], meta[["highs_version"]], "\n")
