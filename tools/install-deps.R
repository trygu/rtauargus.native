# CRAN dependencies of the front-end: the Imports plus the Suggests used
# by the vignettes and the test suite. tauargusengine is not on CRAN; it
# is installed from the staged source package by the workflow.
#
# Lives in a file (rather than an Rscript -e argument) because the
# Windows runner mangles multi-line single-quoted -e scripts.

pkgs <- c(
  "purrr", "dplyr", "tidyr", "data.table", "gdata", "stringr",
  "rlang", "zoo", "sdcHierarchies", "igraph", "lifecycle",
  "testthat", "knitr", "rmarkdown", "visNetwork", "tictoc"
)

install.packages(pkgs, repos = "https://cloud.r-project.org")

# install.packages does not signal per-package failures as an error;
# verify explicitly so a broken dependency fails the job here, not in a
# later, less obvious step.
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1),
                        quietly = TRUE)]
if (length(missing) > 0L) {
  stop("failed to install: ", paste(missing, collapse = ", "))
}
