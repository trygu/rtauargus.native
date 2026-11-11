# 0.7.0 error contract for the R front-end.
#
# - Errors signalled by the native binding (message prefix
#   "Tau-Argus native error") are translated by the batch dispatch
#   into the condition class TauArgusNativeError, a subclass of
#   BatchError, carrying the numeric code (0 when the binding
#   reports none).
# - A failing batch survives Rscript as a non-zero exit with the
#   message (including any native code) on stderr.
# - Native return values the 0.6.x front-end ignored (hierarchical
#   codelist, restore setters) now stop the batch.

fx <- test_path("fixtures", "engine")

run_arb <- function(lines) {
  arb <- tempfile(fileext = ".arb")
  on.exit(unlink(arb), add = TRUE)
  writeLines(lines, arb)
  suppressMessages(rtauargus.native:::run_engine_batch(arb))
}

micro_batch <- function(extra = NULL) {
  c(
    sprintf('<OPENMICRODATA> "%s"', file.path(fx, "ex.asc")),
    sprintf('<OPENMETADATA> "%s"', file.path(fx, "ex.rda")),
    '<SPECIFYTABLE> "Size""Reg"|"Amt"||',
    "<SAFETYRULE> NK(2,50)|NK(0,0)",
    "<READMICRODATA>",
    extra,
    "<GOINTERACTIVE>"
  )
}

# Run `expr` with rta_parse_rda patched to corrupt the parsed metadata.
# The parser only produces whole-number fields, so values rejected by
# the native input guards (fractions) can only be injected here.
with_corrupt_metadata <- function(corrupt, expr) {
  orig <- rtauargus.native:::rta_parse_rda
  assignInNamespace("rta_parse_rda", function(path) {
    meta <- orig(path)
    corrupt(meta)
    meta
  }, ns = "rtauargus.native")
  on.exit(
    assignInNamespace("rta_parse_rda", orig, ns = "rtauargus.native"),
    add = TRUE)
  expr
}

# Run `expr` with a native setter shadowed in the rtauargus.native
# namespace (in the staged package the binding wrappers are defined
# there; engine_run.R resolves free variables through the namespace
# environment). The original binding is restored on exit.
with_shadowed_native <- function(fun_name, fake, expr) {
  ns <- asNamespace("rtauargus.native")
  orig <- get(fun_name, envir = ns)
  assign(fun_name, fake, envir = ns)
  on.exit(assign(fun_name, orig, envir = ns), add = TRUE)
  expr
}

# -- translation (unit) ------------------------------------------------------

test_that("native error messages translate to TauArgusNativeError", {
  e <- tryCatch(
    stop("Tau-Argus native error: argument 'value' must be a finite whole number"),
    error = identity)
  cond <- rtauargus.native:::rta_native_error_condition(e)
  expect_s3_class(cond, "TauArgusNativeError")
  expect_s3_class(cond, "BatchError")
  expect_s3_class(cond, "condition")
  expect_equal(cond$code, 0L)
  expect_match(conditionMessage(cond), "finite whole number")
})

test_that("native error codes are parsed from the message", {
  e <- tryCatch(
    stop("Tau-Argus native error: FullJJ failed (code 2: suboptimal)"),
    error = identity)
  expect_equal(rtauargus.native:::rta_native_error_condition(e)$code, 2L)

  e <- tryCatch(stop("Tau-Argus native error (code -9)"), error = identity)
  expect_equal(rtauargus.native:::rta_native_error_condition(e)$code, -9L)
})

test_that("non-native errors are not translated", {
  e <- tryCatch(stop("some other failure"), error = identity)
  expect_null(rtauargus.native:::rta_native_error_condition(e))
})

# -- end-to-end: typed condition through the batch dispatch ------------------

test_that("a native input-guard failure surfaces as TauArgusNativeError", {
  err <- with_corrupt_metadata(
    function(meta) {
      meta$variables[[1L]]$var_len <- 1.5  # fractional: guard rejects it
      invisible(meta)
    },
    tryCatch(run_arb(micro_batch()), error = identity))
  expect_s3_class(err, "TauArgusNativeError")
  expect_s3_class(err, "BatchError")
  expect_equal(err$code, 0L)
  expect_match(conditionMessage(err), "finite whole number")
})

test_that("batch errors are still BatchError, not translated", {
  err <- tryCatch(
    run_arb(micro_batch('<SUPPRESS> CKM(1) "no_such_ptable.freq"')),
    error = identity)
  expect_s3_class(err, "BatchError")
  expect_false(inherits(err, "TauArgusNativeError"))
  expect_match(conditionMessage(err), "SetCellKeyValuesFreq")
})

# -- phase 5.2: previously ignored native returns -----------------------------

test_that("a failing hierarchical codelist stops the batch", {
  err <- with_corrupt_metadata(
    function(meta) {
      meta$variables[[1L]]$hierarchical <- 2L  # HIER_FILE
      meta$variables[[1L]]$hier_file_name <- file.path(fx, "no_such_hrc.hrc")
      meta$variables[[1L]]$leading_string <- "1."
      invisible(meta)
    },
    tryCatch(run_arb(micro_batch()), error = identity))
  expect_s3_class(err, "BatchError")
  expect_match(conditionMessage(err), "SetHierarchicalCodelist")
})

test_that("restore setter failures stop the batch", {
  eng <- run_arb(micro_batch())
  backup <- eng$e_backup_cell_statuses(0L)
  # The fixture has NK(2,50)-unsafe cells, so at least one status is
  # restored (unknown/empty/empty-nonstructural are skipped).
  expect_true(any(backup$statuses == rtauargus.native:::CS_UNSAFE_RULE))

  fake <- function(...) FALSE
  err <- with_shadowed_native("tauargus_set_table_cell_status_cell", fake,
    tryCatch(eng$e_restore_cell_statuses(0L, backup), error = identity))
  expect_s3_class(err, "BatchError")
  expect_match(conditionMessage(err), "SetTableCellStatus")

  fake2 <- function(...) FALSE
  err2 <- with_shadowed_native("tauargus_set_table_cell_protection_levels",
    fake2,
    tryCatch(eng$e_restore_cell_statuses(0L, backup), error = identity))
  expect_s3_class(err2, "BatchError")
  expect_match(conditionMessage(err2), "SetTableCellProtectionLevels")
})

# -- Rscript survival -----------------------------------------------------------

test_that("a failing batch exits Rscript non-zero with the message", {
  arb <- tempfile(fileext = ".arb")
  on.exit(unlink(arb), add = TRUE)
  writeLines(micro_batch('<SUPPRESS> CKM(1) "no_such_ptable.freq"'), arb)

  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    "library(rtauargus.native)",
    sprintf("rtauargus.native:::run_engine_batch('%s')", arb)
  ), script)

  err_file <- tempfile(fileext = ".err")
  on.exit(unlink(err_file), add = TRUE)
  res <- suppressWarnings(system2(
    R.home("Rscript"), shQuote(script),
    stdout = tempfile(), stderr = err_file, wait = TRUE))
  expect_gt(res, 0L)
  expect_match(paste(readLines(err_file, warn = FALSE)),
               "SetCellKeyValuesFreq")
})
