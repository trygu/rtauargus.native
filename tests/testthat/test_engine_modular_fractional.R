# MOD (modular) suppression on a hierarchical table with fractional
# cell values.
#
# The RDS fixtures are serialized from the exact data frames returned by
# as.data.frame(arrow::read_parquet(...)) for the SSB report against
# rtauargus.native 0.6 (engine pin c147eca). Plain read.csv changes
# 160 response doubles at the last bit and changes the MOD result.
# This two-level 110x14 table has values up to about 9e9 carrying
# machine-precision fractional parts and 429 a priori
# secrets (secret_var) with the same cells marked without protection
# level (secret_no_pl), protected with MOD(1,5,0,0,0). The integer
# variant of the same table is the control.
#
# On 0.6 the fractional variant aborted the R session on Windows
# ("R Session Aborted") while the integer variant ran; on macOS both
# ran. The test pins the contract: the batch completes, the 429 a
# priori secrets stay secret, the remaining cells split into visible
# and secondary, and the published values are finite.

fx <- test_path("fixtures", "modular")

run_modular <- function(variant, dir, truncate_response = FALSE) {
  tab <- readRDS(file.path(fx, paste0("modular_", variant, ".rds")))
  if (truncate_response) {
    # A controlled comparison: keep the decimal input's exact rows,
    # flags and hierarchies; change only the response's fractional part.
    tab$response <- trunc(tab$response)
  }
  # Copy the hierarchy files to a scratch dir: tab_rtauargus writes a
  # uniformized *_unif.hrc next to the input hrc.
  file.copy(file.path(fx, "hier1.hrc"), file.path(dir, "hier1.hrc"))
  file.copy(file.path(fx, "hier2.hrc"), file.path(dir, "hier2.hrc"))
  tab_rtauargus(
    tab,
    dir_name = dir,
    files_name = "sy1",
    explanatory_vars = c("var1", "var2"),
    hrc = c(var1 = file.path(dir, "hier1.hrc"),
            var2 = file.path(dir, "hier2.hrc")),
    secret_var = "primary",
    secret_no_pl = "no_pl",
    value = "response",
    freq = "n_contr",
    totcode = c(var1 = "T", var2 = "T"),
    split_tab = FALSE,
    suppress = "MOD(1,5,0,0,0)"
  )
}

test_that("MOD completes on the fractional hierarchical table", {
  dir <- file.path(tempdir(), "tauargus_modular_dec")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- run_modular("dec", dir)

  expect_equal(nrow(res), 1653L)
  st <- table(res$Status)
  cat("MOD Arrow-equivalent fractional run on ", Sys.info()[["sysname"]],
      ": ", paste(names(st), as.integer(st), collapse = " "), "\n", sep = "")
  # The 429 a priori secrets stay secret.
  expect_equal(st[["B"]], 429L)
  # The remaining 1224 cells are visible or secondary. A status that never
  # occurs is absent from the table (reads as NA), so count it directly
  # rather than through the table.
  expect_equal(sum(res$Status == "V") + sum(res$Status == "D"), 1224L)
  # The published values are finite.
  v <- res[res$Status == "V", "response"]
  expect_true(all(is.finite(v)))
  expect_true(file.exists(file.path(dir, "sy1.csv")))
})

test_that("MOD completes on the integer variant of the same table", {
  dir <- file.path(tempdir(), "tauargus_modular_int")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  res <- run_modular("int", dir)

  expect_equal(nrow(res), 1653L)
  st <- table(res$Status)
  expect_equal(st[["B"]], 429L)
  expect_equal(sum(res$Status == "V") + sum(res$Status == "D"), 1224L)
  expect_true(all(is.finite(res$response[res$Status == "V"])))
})

test_that("MOD compares decimal and truncated values on identical input rows", {
  dec_dir <- tempfile("tauargus_modular_pair_dec_")
  whole_dir <- tempfile("tauargus_modular_pair_whole_")
  dir.create(dec_dir)
  dir.create(whole_dir)
  on.exit(unlink(c(dec_dir, whole_dir), recursive = TRUE), add = TRUE)

  dec <- run_modular("dec", dec_dir)
  whole <- run_modular("dec", whole_dir, truncate_response = TRUE)
  expect_equal(nrow(dec), 1653L)
  expect_equal(nrow(whole), 1653L)
  expect_equal(table(dec$Status)[["B"]], 429L)
  expect_equal(table(whole$Status)[["B"]], 429L)
  expect_true(all(is.finite(dec$response[dec$Status == "V"])))
  expect_true(all(is.finite(whole$response[whole$Status == "V"])))
  cat("MOD controlled decimal/truncated pair completed on ",
      Sys.info()[["sysname"]], "\n", sep = "")
})
