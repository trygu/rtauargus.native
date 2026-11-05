source("tools/check-cran-log.R")
run_case <- function(lines, accepted) {
  path <- tempfile()
  on.exit(unlink(path))
  writeLines(lines, path)
  result <- tryCatch(check_cran_log(path), error=identity)
  stopifnot(identical(!inherits(result, "error"), accepted))
}
run_case(c("* checking CRAN incoming feasibility ... NOTE",
           "* checking compiled code ... OK", "* DONE", "Status: 1 NOTE"), TRUE)
run_case(c("* checking installation ... WARNING", "* DONE"), FALSE)
run_case(c("* checking tests ... ERROR", "* DONE"), FALSE)
run_case(c("* checking compiled code ... NOTE", "* DONE"), FALSE)
run_case(c("* DONE", "Status: 1 WARNING, 1 NOTE"), FALSE)
run_case("* checking tests ...", FALSE)
run_case(c("* checking CRAN incoming feasibility ... NOTE",
           "Authors@R field should be a call to person(), or combine such calls.",
           "* DONE"), FALSE)
run_case(c("* checking CRAN incoming feasibility ... NOTE",
           "Package has a VignetteBuilder field but no prebuilt vignette index.",
           "* DONE"), FALSE)
run_case(c("* checking CRAN incoming feasibility ... NOTE",
           "but no 'build/stage23.rdb' file.", "* DONE"), FALSE)
cat("CRAN check-log gate tests passed\n")
