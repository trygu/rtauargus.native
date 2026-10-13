# Table-input (.tab) tests for the native engine driver.
#
# Fixture: fixtures/engine/tab.tab (9 rows, ';' separated, comma decimals)
# + tab.rda (11 variables: 2 recodeable codes with total code, response
# with 2 decimals, frequency, shadow, cost, 2x top-n, status, 2x
# protection levels).
#
# Expected values were verified against the Python reference engine.

fx <- test_path("fixtures", "engine")

run_arb <- function(lines) {
  arb <- tempfile(fileext = ".arb")
  on.exit(unlink(arb), add = TRUE)
  writeLines(lines, arb)
  suppressMessages(rtauargus:::run_engine_batch(arb))
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

eng_table <- run_arb(table_batch())

# 1-based linear cell index; code order: Total, A, B, 99 (missing last)
lin <- function(row, col) (row - 1L) * 4L + col

test_that("table input batch reads the .tab values", {
  expect_equal(tauargusengine::tauargus_get_total_table_size(eng_table$tau, 0L)[1L], 16)

  # leaf cells (comma decimals in the .tab: 10000,00 etc.)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(2L, 2L) - 1L), 10000)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(2L, 3L) - 1L), 9000)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(3L, 2L) - 1L), 9000)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(3L, 3L) - 1L), 10000)

  # totals
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(1L, 2L) - 1L), 19000)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(1L, 3L) - 1L), 19000)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(2L, 1L) - 1L), 19000)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(3L, 1L) - 1L), 19000)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_value(eng_table$tau, 0L, lin(1L, 1L) - 1L), 38000)

  # missing-code row and column are empty
  expect_equal(
    tauargusengine::tauargus_get_table_cell_status(eng_table$tau, 0L, lin(4L, 2L) - 1L),
    rtauargus:::CS_EMPTY)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_status(eng_table$tau, 0L, lin(2L, 4L) - 1L),
    rtauargus:::CS_EMPTY)
})

test_that("READTABLE 1T keeps the file statuses as safe-manual", {
  # every cell in tab.tab carries the status character "S"
  expect_equal(
    tauargusengine::tauargus_get_table_cell_status(eng_table$tau, 0L, lin(1L, 1L) - 1L),
    rtauargus:::CS_SAFE_MANUAL)
  expect_equal(
    tauargusengine::tauargus_get_table_cell_status(eng_table$tau, 0L, lin(2L, 2L) - 1L),
    rtauargus:::CS_SAFE_MANUAL)
})

test_that("the 22-field cell accessor exposes response, frequency, shadow", {
  # (A, A) = row 2, col 2; dim_index is 0-based (0 = total code)
  cell <- tauargusengine::tauargus_get_table_cell(eng_table$tau, 0L, c(1L, 1L), 0L)
  expect_true(cell$ok)
  expect_equal(cell$resp, 10000)
  expect_equal(cell$freq, 2)
  expect_equal(cell$shadow, 10000)
  expect_equal(cell$status, rtauargus:::CS_SAFE_MANUAL)

  # grand total
  gt <- tauargusengine::tauargus_get_table_cell(eng_table$tau, 0L, c(0L, 0L), 0L)
  expect_equal(gt$resp, 38000)
  expect_equal(gt$freq, 8)
})

test_that("inconsistent top-n values are rejected", {
  bad_tab <- tempfile(fileext = ".tab")
  on.exit(unlink(bad_tab), add = TRUE)
  # top1 + top2 (19000) exceeds the cell value (10000)
  writeLines(
    c('"Total";"Total";38000,00;8;38000,00;38000,00;8000,00;7000,00;S;0,01;0,01"',
      '"Total";"A";19000,00;4;19000,00;19000,00;6000,00;5000,00;S;0,01;0,01',
      '"Total";"B";19000,00;4;19000,00;19000,00;6000,00;5000,00;S;0,01;0,01',
      '"A";"Total";19000,00;4;19000,00;19000,00;6000,00;5000,00;S;0,01;0,01',
      '"A";"A";10000,00;2;10000,00;10000,00;10000,00;9000,00;S;0,01;0,01',
      '"A";"B";9000,00;2;9000,00;9000,00;5000,00;4000,00;S;0,01;0,01',
      '"B";"Total";19000,00;4;19000,00;19000,00;6000,00;5000,00;S;0,01;0,01',
      '"B";"A";9000,00;2;9000,00;9000,00;5000,00;4000,00;S;0,01;0,01',
      '"B";"B";10000,00;2;10000,00;10000,00;6000,00;4000,00;S;0,01;0,01'),
    bad_tab)

  expect_error(
    run_arb(table_batch(tab_file = bad_tab)),
    class = "BatchError"
  )
})

test_that("OPENTABLEDATA before a data file is rejected by the parser", {
  # (parser level: OPENMETADATA needs a data file first)
  p <- tempfile(fileext = ".arb")
  on.exit(unlink(p), add = TRUE)
  writeLines(c(
    sprintf('<OPENMETADATA> "%s"', file.path(fx, "tab.rda")),
    '<OPENTABLEDATA> "%s"', file.path(fx, "tab.tab")
  ), p)
  expect_error(rtauargus:::parse_batch(p), class = "BatchError")
})
