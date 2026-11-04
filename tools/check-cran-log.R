# Fail CI on incomplete checks, warnings/errors, or unsafe native-code notes.
check_cran_log <- function(path) {
  lines <- readLines(path, warn=FALSE)
  if (!any(grepl("^\\* DONE", lines))) stop("CRAN check did not finish: ", path)
  failed <- grepl("^\\* .*\\.\\.\\. (ERROR|WARNING)([[:space:]]|$)", lines) |
    grepl("^Status:.*(ERROR|WARNING)", lines) |
    grepl("^\\* checking compiled code \\.\\.\\. NOTE", lines) |
    grepl("Authors@R field should|no prebuilt vignette index|no 'build/stage23[.]rdb'", lines)
  if (any(failed)) stop(paste(lines[failed], collapse="\n"), call.=FALSE)
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  if (length(args) != 1L) stop("usage: Rscript tools/check-cran-log.R 00check.log")
  check_cran_log(args[[1L]])
  cat("CRAN source check passed the warning and native-code gates\n")
}
