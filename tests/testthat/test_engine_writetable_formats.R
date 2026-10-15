# WRITETABLE output formats: per-format argument vector (legacy SaveTable).
#
# The legacy batch processor (engine commit 307b977^,
# src/tauargus/model/batch.java writeTableBatch + SaveTable.java
# writeTable) maps each WRITETABLE output type to a specific native
# call:
#
#   type 1 CSV:         WriteCSV(tab, file, embed, identity, resp)
#   type 2 pivot:       WriteCellRecords(tab, file, 0, FALSE, SE,
#                                        firstLine, AS, embed, resp)
#   type 3 code value:  WriteCellRecords(tab, file, 0, FALSE, SE,
#                                        firstLine or "", AS, embed, resp)
#   type 4 SBS:         WriteCellRecords(tab, file, 1, HL, TRUE,
#                                        "", TRUE, embed, resp)
#
# Per-command defaults (batch.java): embed on, add-status /
# suppress-empty / hierarchical off, varnames-on-first-line on for
# pivot only.  Before the fix the port collapsed types 2-7 into one
# WriteCellRecords call (SBS=1, SuppressEmpty=FALSE, ShowUnsafe=FALSE,
# no header): the SBS export masked suppressed cells as "x" and wrote
# the empty rows, and the pivot and code value exports carried no
# header.  Types 6 (JJ) and 7 (CKM) have no native equivalent of the
# legacy writers and are not implemented.

fx <- test_path("fixtures", "engine")

run_arb <- function(lines) {
  arb <- tempfile(fileext = ".arb")
  on.exit(unlink(arb), add = TRUE)
  writeLines(lines, arb)
  suppressMessages(rtauargus.native:::run_engine_batch(arb))
}

table_batch <- function(tab_file = file.path(fx, "tab.tab"),
                        rda_file = file.path(fx, "tab.rda")) {
  c(
    sprintf('<OPENTABLEDATA> "%s"', tab_file),
    sprintf('<OPENMETADATA> "%s"', rda_file),
    '<SPECIFYTABLE> "Size""Region"|"Var2"|"Var2_Shadow"|"Var2"',
    "<READTABLE> 1T",
    "<GOINTERACTIVE>"
  )
}

# 1-based linear cell index; code order: Total, A, B, 99 (missing last)
lin <- function(row, col) (row - 1L) * 4L + col

PIVOT_HEADER <- '"Size","Region","Var2"'

wt <- function(eng, type, options = character(0), out) {
  eng$write_table(rtauargus.native:::new_write_table(
    tab_no = 1L, output_type = type, options = options, file = out))
}

test_that("SBS (type 4) writes values for unsafe cells and skips empty rows", {
  eng <- run_arb(table_batch())
  tau <- eng$tau
  # Mark (A, A) = 10000 primary-unsafe.
  tauargusengine::tauargus_set_table_cell_status_cell(
    tau, 0L, lin(2L, 2L) - 1L, rtauargus.native:::CS_UNSAFE_MANUAL)

  out <- tempfile(fileext = ".csv")
  wt(eng, 4L, character(0), out)
  txt <- readLines(out, warn = FALSE)

  # The 4 x 4 grid has 7 empty cells (missing-code row and column);
  # the SBS export writes only the 9 non-empty rows.
  expect_length(txt, 9L)
  # No "x" mask anywhere: the unsafe cell keeps its value.
  expect_false(any(grepl("(^|,)x(,|$)", txt)))
  aa <- grep("^\"A\",\"A\",", txt, value = TRUE)
  expect_length(aa, 1L)
  expect_match(aa, '^"A","A",10000\\.00,2,B')
})

test_that("SBS (type 4) matches the legacy argument vector", {
  eng <- run_arb(table_batch())
  tau <- eng$tau
  out <- tempfile(fileext = ".csv")
  wt(eng, 4L, character(0), out)
  oracle <- tempfile(fileext = ".csv")
  tauargusengine::tauargus_write_cell_records(
    tau, 0L, oracle, 1L, FALSE, TRUE, "", TRUE, TRUE, 0L)
  expect_equal(readBin(out, "raw", file.size(out)),
               readBin(oracle, "raw", file.size(oracle)))
})

test_that("pivot (type 2) writes the variable-name header", {
  eng <- run_arb(table_batch())
  out <- tempfile(fileext = ".csv")
  wt(eng, 2L, character(0), out)
  lines <- readLines(out, warn = FALSE)
  expect_equal(lines[1L], PIVOT_HEADER)

  oracle <- tempfile(fileext = ".csv")
  tauargusengine::tauargus_write_cell_records(
    eng$tau, 0L, oracle, 0L, FALSE, FALSE, PIVOT_HEADER, FALSE, TRUE, 0L)
  expect_equal(readBin(out, "raw", file.size(out)),
               readBin(oracle, "raw", file.size(oracle)))
})

test_that("pivot (type 2) with AS+ adds the Status column name", {
  eng <- run_arb(table_batch())
  out <- tempfile(fileext = ".csv")
  wt(eng, 2L, "+AS", out)
  lines <- readLines(out, warn = FALSE)
  expect_equal(lines[1L], paste0(PIVOT_HEADER, ',"Status"'))

  oracle <- tempfile(fileext = ".csv")
  tauargusengine::tauargus_write_cell_records(
    eng$tau, 0L, oracle, 0L, FALSE, FALSE,
    paste0(PIVOT_HEADER, ',"Status"'), TRUE, TRUE, 0L)
  expect_equal(readBin(out, "raw", file.size(out)),
               readBin(oracle, "raw", file.size(oracle)))
})

test_that("code value (type 3) has no header by default", {
  eng <- run_arb(table_batch())
  out <- tempfile(fileext = ".csv")
  wt(eng, 3L, character(0), out)
  lines <- readLines(out, warn = FALSE)
  expect_match(lines[1L], '^"Total","Total",')

  oracle <- tempfile(fileext = ".csv")
  tauargusengine::tauargus_write_cell_records(
    eng$tau, 0L, oracle, 0L, FALSE, FALSE, "", FALSE, TRUE, 0L)
  expect_equal(readBin(out, "raw", file.size(out)),
               readBin(oracle, "raw", file.size(oracle)))
})

test_that("code value (type 3) with FL+ writes the header", {
  eng <- run_arb(table_batch())
  out <- tempfile(fileext = ".csv")
  wt(eng, 3L, "+FL", out)
  lines <- readLines(out, warn = FALSE)
  expect_equal(lines[1L], PIVOT_HEADER)

  oracle <- tempfile(fileext = ".csv")
  tauargusengine::tauargus_write_cell_records(
    eng$tau, 0L, oracle, 0L, FALSE, FALSE, PIVOT_HEADER, FALSE, TRUE, 0L)
  expect_equal(readBin(out, "raw", file.size(out)),
               readBin(oracle, "raw", file.size(oracle)))
})

test_that("CSV (type 1) honours QU- (no embedded quotes)", {
  eng <- run_arb(table_batch())
  out <- tempfile(fileext = ".csv")
  wt(eng, 1L, "-QU", out)
  expect_false(grepl("\"", paste(readLines(out, warn = FALSE), collapse = "")))
})

test_that("JJ (type 6) and CKM (type 7) are not implemented", {
  eng <- run_arb(table_batch())
  out <- tempfile(fileext = ".csv")
  expect_error(wt(eng, 6L, character(0), out),
               "not implemented", class = "BatchError")
  expect_error(wt(eng, 7L, character(0), out),
               "not implemented", class = "BatchError")
})
