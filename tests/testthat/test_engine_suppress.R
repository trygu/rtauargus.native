# Secondary suppression (MOD / OPT / RND) and audit tests for the native
# engine driver, run end-to-end on the microdata fixture.
#
# Expected values were verified against the Python reference engine
# (pytauargus) running the same native libraries; the MOD/OPT audit files
# were byte-compared.

fx <- test_path("fixtures", "engine")
out_dir <- tempfile("rta_engine_test_")
dir.create(out_dir)
on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

# Note: messages are NOT suppressed here; expect_message() relies on them.
run_arb <- function(lines) {
  arb <- tempfile(fileext = ".arb")
  on.exit(unlink(arb), add = TRUE)
  writeLines(lines, arb)
  rtauargus:::run_engine_batch(arb)
}

# 1-based linear cell index; Table 1 code order:
# rows: Total, "  1", "  2", "  3", "  9"; cols: Total, " A", " B", " X"
lin <- function(row, col) (row - 1L) * 4L + col

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

test_that("MOD suppression sets secondary cells and the audit reflects it", {
  audit <- file.path(out_dir, "audit_mod.tab")
  eng <- expect_message(
    run_arb(micro_batch(c(
      "<SUPPRESS> MOD(1)",
      sprintf('<WRITETABLE> (1, 5, AR+, "%s")', audit)
    ))),
    "MOD suppress"
  )

  # the audit file has one row per cell (5 x 4 = 20) with 17 AR+ fields
  lines <- readLines(audit, warn = FALSE)
  expect_length(lines, 20)
  expect_true(all(vapply(strsplit(lines, ";", fixed = TRUE), length, integer(1)) == 17L))

  # AHiTaS marks the column-A total secondary (verified vs the reference);
  # its row carries the "M" status symbol and the AR+ interval columns
  expect_identical(lines[2],
    '"Total";" A";21000.00;6;21000.00;1.95;5000.00;5000.00;M;0.01;0.01;-0.00;40000.00;40000.00;100.00;90.48;190.48')
  # grand total stays safe and untouched
  expect_identical(lines[1],
    '"Total";"Total";40000.00;10;40000.00;2.40;5000.00;5000.00;S;0.01;0.01;0.00;0.00;0.00;0;0;0')
  # empty cells are audited as "E"
  expect_identical(lines[4],
    '"Total";" X";0.00;0;0.00;0.00;0.00;0.00;E;0.00;0.00;0.00;0.00;0.00;0;0;0')

  # the secondary cell status is set in the table itself
  expect_equal(
    tauargusengine::tauargus_get_table_cell_status(eng$tau, 0L, lin(1L, 2L) - 1L),
    rtauargus:::CS_SECONDARY_UNSAFE)
  # the grand total value is preserved by the modification
  expect_equal(tauargusengine::tauargus_get_table_cell_value(eng$tau, 0L, 0L), 40000)
})

test_that("OPT suppression sets secondary cells and audits", {
  audit <- file.path(out_dir, "audit_opt.tab")
  eng <- expect_message(
    run_arb(micro_batch(c(
      "<SUPPRESS> OPT(1)",
      sprintf('<WRITETABLE> (1, 5, AR+, "%s")', audit)
    ))),
    "OPT suppress"
  )

  lines <- readLines(audit, warn = FALSE)
  expect_length(lines, 20)
  # same secondary cell and same audit rows as the reference engine
  expect_identical(lines[1],
    '"Total";"Total";40000.00;10;40000.00;2.40;5000.00;5000.00;S;0.01;0.01;0.00;0.00;0.00;0;0;0')
  expect_identical(lines[2],
    '"Total";" A";21000.00;6;21000.00;1.95;5000.00;5000.00;M;0.01;0.01;-0.00;40000.00;40000.00;100.00;90.48;190.48')
  expect_equal(
    tauargusengine::tauargus_get_table_cell_status(eng$tau, 0L, lin(1L, 2L) - 1L),
    rtauargus:::CS_SECONDARY_UNSAFE)
})

test_that("RND rounds to multiples of the base and preserves additivity", {
  csv <- file.path(out_dir, "rnd.csv")
  eng <- expect_message(
    run_arb(micro_batch(c(
      "<SUPPRESS> RND(1, 10000)",
      sprintf('<WRITETABLE> (1, 1,, "%s")', csv)
    ))),
    "RND suppress"
  )

  # the writer terminates the file with a blank line; drop it
  lines <- readLines(csv, warn = FALSE)
  lines <- lines[nzchar(lines)]
  expect_length(lines, 6)
  # header + 5 rows (total, 1, 2, 3, missing 9) x 5 columns (label + 4)
  expect_identical(lines[1], ',"Total"," A"," B"," X"')
  expect_identical(lines[2], '"Total",40000.00,20000.00,20000.00,-')
  expect_identical(lines[3], '"  1",10000.00,0.00,10000.00,-')
  expect_identical(lines[4], '"  2",20000.00,10000.00,10000.00,-')
  expect_identical(lines[5], '"  3",10000.00,10000.00,0.00,-')
  expect_identical(lines[6], '"  9",-,-,-,-')

  # every rounded value is a multiple of the base ("-": missing code)
  rounded <- read.table(csv, header = FALSE, sep = ",",
                        colClasses = "character", strip.white = TRUE)
  nums <- suppressWarnings(as.numeric(as.matrix(rounded)[2:6, 2:5]))
  expect_true(all(nums[!is.na(nums)] %% 10000 == 0))

  # the raw (unrounded) responses are left untouched in the table
  expect_equal(tauargusengine::tauargus_get_table_cell_value(eng$tau, 0L, lin(2L, 2L) - 1L),
               10000) # (1, A) raw
  expect_equal(tauargusengine::tauargus_get_table_cell_value(eng$tau, 0L, lin(2L, 3L) - 1L),
               4500)  # (1, B) raw
})

test_that("RND rejects a base below the minimum with a clean error", {
  # max cell value 40000 -> minimum base 10^4 (see computeMinRoundBase);
  # a too-small base used to produce a malformed JJ file and crash the
  # rounder instead of failing cleanly.
  expect_error(
    run_arb(micro_batch("<SUPPRESS> RND(1, 100)")),
    class = "BatchError",
    "Rounding base 100 too small; minimum 10000 required"
  )
})

test_that("GH suppression is rejected in headless mode", {
  expect_error(
    run_arb(micro_batch("<SUPPRESS> GH(1, 100, 10)")),
    class = "BatchError",
    "not supported in headless mode"
  )
})
