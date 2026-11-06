# MOD (modular) suppression on a hierarchical table with fractional
# cell values.
#
# The fixture is the SSB scenario reported against rtauargus.native 0.6
# (engine pin c147eca): a two-level 110x14 table with values up to
# about 9e9 carrying machine-precision fractional parts, 429 a priori
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

run_modular <- function(variant, dir) {
  tab <- read.csv(
    file.path(fx, paste0("modular_", variant, ".csv")),
    stringsAsFactors = FALSE
  )
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
  # The 429 a priori secrets stay secret.
  expect_equal(st[["B"]], 429L)
  # The remaining 1224 cells split into visible and secondary, and the
  # protection actually suppresses some of them.
  expect_equal(sum(st[c("D", "V")]), 1224L)
  expect_gte(st[["D"]], 100L)
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
  expect_equal(sum(st[c("D", "V")]), 1224L)
  expect_gte(st[["D"]], 100L)
  expect_true(all(is.finite(res$response[res$Status == "V"])))
})
