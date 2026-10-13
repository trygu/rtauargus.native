# End-to-end microdata tests for the native engine driver.
#
# Fixture: fixtures/engine/ex.asc (10 fixed-width records) + ex.rda
#   Size (3) in {1,2,3}, Reg (2) in {A,B}, Ind (2, hierarchical 1 1)
#   in {11,12,21}, Amt (8, 2 decimals).
# Table 1 = Size x Reg, Table 2 = Reg x Ind, response Amt.
#
# Safety rule NK(2,50) is the legacy *dominance* rule: a cell is unsafe
# when its two largest contributing records exceed 50 % of the cell value.
#
# All expected values below were verified against the Python reference
# engine (pytauargus) running the same native libraries.

fx <- test_path("fixtures", "engine")

run_arb <- function(lines) {
  arb <- tempfile(fileext = ".arb")
  on.exit(unlink(arb), add = TRUE)
  writeLines(lines, arb)
  suppressMessages(rtauargus:::run_engine_batch(arb))
}

# 1-based linear cell index for the (row-code, col-code) position; the
# total code is "" and missing codes sort last in the code lists.
cell_at <- function(eng, tab, row_code, col_code) {
  spec <- eng$tables[[tab + 1L]]$spec
  meta <- eng$metadata
  vi1 <- rtauargus:::meta_index_of(meta, spec$exp_vars[1L]) + 1L
  vi2 <- rtauargus:::meta_index_of(meta, spec$exp_vars[2L]) + 1L
  code_pos <- function(vi, code) {
    n <- as.integer(tauargusengine::tauargus_get_var_number_of_codes(eng$tau, vi - 1L)[2L])
    for (i in seq_len(n)) {
      r <- tauargusengine::tauargus_get_var_code(eng$tau, vi - 1L, i - 1L)
      if (isTRUE(r$ok) && trimws(r$code_string) == trimws(code))
        return(i - 1L)
    }
    stop("code '", code, "' not found")
  }
  i1 <- code_pos(vi1, row_code)
  i2 <- code_pos(vi2, col_code)
  ncol <- as.integer(tauargusengine::tauargus_get_var_number_of_codes(eng$tau, vi2 - 1L)[2L])
  (i1 * ncol + i2) + 1L
}

val_at <- function(eng, tab, r, c)
  tauargusengine::tauargus_get_table_cell_value(eng$tau, tab, cell_at(eng, tab, r, c) - 1L)
st_at <- function(eng, tab, r, c)
  tauargusengine::tauargus_get_table_cell_status(eng$tau, tab, cell_at(eng, tab, r, c) - 1L)

micro_batch <- function(extra = NULL) {
  c(
    sprintf('<OPENMICRODATA> "%s"', file.path(fx, "ex.asc")),
    sprintf('<OPENMETADATA> "%s"', file.path(fx, "ex.rda")),
    '<SPECIFYTABLE> "Size""Reg"|"Amt"||',
    "<SAFETYRULE> NK(2,50)|NK(0,0)",
    '<SPECIFYTABLE> "Reg""Ind"|"Amt"||',
    "<SAFETYRULE> NK(2,50)|NK(0,0)",
    "<READMICRODATA>",
    extra,
    "<GOINTERACTIVE>"
  )
}

eng_micro <- run_arb(micro_batch())

test_that("microdata batch computes both tables", {
  # Table 1: Size (5 codes incl. total + missing) x Reg (4) = 20 cells
  expect_equal(tauargusengine::tauargus_get_total_table_size(eng_micro$tau, 0L)[1L], 20)
  # Table 2: Reg (4) x Ind (7 codes incl. total + missing) = 28 cells
  expect_equal(tauargusengine::tauargus_get_total_table_size(eng_micro$tau, 1L)[1L], 28)

  # Table 1 leaf cells (records: 1A:2x5000 1B:1x4500 2A:2x4500
  #                        2B:1x4500 3A:2x1000 3B:2x5000)
  expect_equal(val_at(eng_micro, 0L, "  1", " A"), 10000)
  expect_equal(val_at(eng_micro, 0L, "  1", " B"), 4500)
  expect_equal(val_at(eng_micro, 0L, "  2", " A"), 9000)
  expect_equal(val_at(eng_micro, 0L, "  2", " B"), 4500)
  expect_equal(val_at(eng_micro, 0L, "  3", " A"), 2000)
  expect_equal(val_at(eng_micro, 0L, "  3", " B"), 10000)

  # row and column totals and the grand total
  expect_equal(val_at(eng_micro, 0L, "  1", ""), 14500)
  expect_equal(val_at(eng_micro, 0L, "  2", ""), 13500)
  expect_equal(val_at(eng_micro, 0L, "  3", ""), 12000)
  expect_equal(val_at(eng_micro, 0L, "", " A"), 21000)
  expect_equal(val_at(eng_micro, 0L, "", " B"), 19000)
  expect_equal(val_at(eng_micro, 0L, "", ""), 40000)

  # missing-code cells are empty
  expect_equal(val_at(eng_micro, 0L, "  9", ""), 0)
  expect_equal(st_at(eng_micro, 0L, "  9", ""), rtauargus:::CS_EMPTY)
  expect_equal(st_at(eng_micro, 0L, "", " X"), rtauargus:::CS_EMPTY)
})

test_that("NK(2,50) flags dominance of the two largest records", {
  # grand total: top2 = 10000 / 40000 = 25 % -> safe
  expect_equal(st_at(eng_micro, 0L, "", ""), rtauargus:::CS_SAFE)
  # column A total: top2 = 10000 / 21000 = 47.6 % <= 50 -> safe
  expect_equal(st_at(eng_micro, 0L, "", " A"), rtauargus:::CS_SAFE)
  # column B total: top2 = 10000 / 19000 = 52.6 % > 50 -> unsafe
  expect_equal(st_at(eng_micro, 0L, "", " B"), rtauargus:::CS_UNSAFE_RULE)
  # every single/double-record leaf cell is 100 % dominated -> unsafe
  expect_equal(st_at(eng_micro, 0L, "  1", " A"), rtauargus:::CS_UNSAFE_RULE)
  expect_equal(st_at(eng_micro, 0L, "  1", " B"), rtauargus:::CS_UNSAFE_RULE)
  expect_equal(st_at(eng_micro, 0L, "  3", " B"), rtauargus:::CS_UNSAFE_RULE)
})

test_that("WRITETABLE CSV before RND writes raw values, not the (zero) rounded response", {
  # The legacy SaveTable derives the response type from the table state:
  # 0 (raw) until RND has rounded the table. Before RND the rounded
  # response is 0 for every cell, so a hardcoded resp_type=1 produced an
  # all-zero CSV. Native WriteCSVCell with RespType 0 prints the raw value
  # for safe cells, "x" for unsafe cells (ShowUnsafe = false) and "-" for
  # empty cells (matches the Python reference byte-for-byte).
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  run_arb(micro_batch(sprintf('<WRITETABLE> (1,1,,"%s")', csv)))
  lines <- readLines(csv, warn = FALSE)
  lines <- lines[nzchar(lines)]
  expect_equal(lines, c(
    ',"Total"," A"," B"," X"',
    '"Total",40000.00,21000.00,x,-',
    '"  1",x,x,x,-',
    '"  2",x,x,x,-',
    '"  3",x,x,x,-',
    '"  9",-,-,-,-'
  ))
})

test_that("hierarchical variable produces level and leaf codes", {
  # Ind level-1 codes aggregate their level-2 children
  expect_equal(val_at(eng_micro, 1L, "", "1"), 28000)   # 11 + 12
  expect_equal(val_at(eng_micro, 1L, "", "2"), 12000)   # 21
  expect_equal(val_at(eng_micro, 1L, "", "11"), 19000)
  expect_equal(val_at(eng_micro, 1L, "", "12"), 9000)
  expect_equal(val_at(eng_micro, 1L, "", "21"), 12000)
  expect_equal(val_at(eng_micro, 1L, " A", "11"), 19000)
  expect_equal(val_at(eng_micro, 1L, " B", "12"), 9000)
  expect_equal(val_at(eng_micro, 1L, " A", "21"), 2000)
  expect_equal(val_at(eng_micro, 1L, " B", "21"), 10000)
  # structurally empty cells
  expect_equal(val_at(eng_micro, 1L, " A", "12"), 0)
  expect_equal(st_at(eng_micro, 1L, " A", "12"), rtauargus:::CS_EMPTY)
  expect_equal(st_at(eng_micro, 1L, " B", "11"), rtauargus:::CS_EMPTY)
  # row/column totals
  expect_equal(val_at(eng_micro, 1L, " A", ""), 21000)
  expect_equal(val_at(eng_micro, 1L, " B", ""), 19000)
})

test_that("file recode merges source codes into the destination code", {
  # size.grc: 1:2-3  -> Size codes 2 and 3 are recoded to 1
  eng <- run_arb(micro_batch(
    sprintf('<RECODE> 1, Size, "%s"', file.path(fx, "size.grc"))
  ))
  # table 1 rows after the recode: total, 1 (merged), 1 (dest code), 9
  ncol <- 4L # Total A B X
  lin <- function(row, col) (row - 1L) * ncol + col
  # merged row (row 2) carries all records
  expect_equal(tauargusengine::tauargus_get_table_cell_value(eng$tau, 0L, lin(2L, 1L) - 1L), 40000)
  expect_equal(tauargusengine::tauargus_get_table_cell_value(eng$tau, 0L, lin(2L, 2L) - 1L), 21000)
  expect_equal(tauargusengine::tauargus_get_table_cell_value(eng$tau, 0L, lin(2L, 3L) - 1L), 19000)
  # the destination-code row (row 3) itself is empty
  expect_equal(tauargusengine::tauargus_get_table_cell_value(eng$tau, 0L, lin(3L, 2L) - 1L), 0)
})

test_that("truncation recode keeps only the requested level", {
  eng <- run_arb(micro_batch("<RECODE> 2, Ind, 1"))
  # Ind now only has level-1 codes: total, 1, 2, 99
  # (the native index is 0-based, as in meta_index_of)
  expect_equal(
    as.integer(tauargusengine::tauargus_get_var_number_of_codes(
      eng$tau,
      rtauargus:::meta_index_of(eng$metadata, "Ind")
    )[2L]), 4)
  expect_equal(val_at(eng, 1L, "", "1"), 28000)
  expect_equal(val_at(eng, 1L, "", "2"), 12000)
  expect_equal(val_at(eng, 1L, " A", "1"), 19000)
  expect_equal(val_at(eng, 1L, " A", "2"), 2000)
  expect_equal(val_at(eng, 1L, " B", "1"), 9000)
  expect_equal(val_at(eng, 1L, " B", "2"), 10000)
  # the level-2 codes are gone
  expect_error(val_at(eng, 1L, " A", "11"), "not found")
})

test_that("truncation is rejected for non-hierarchical variables", {
  expect_error(
    run_arb(micro_batch("<RECODE> 1, Size, 1")),
    class = "BatchError"
  )
})

test_that("tree recode closes listed parent codes", {
  # indtree.grc: <TREERECODE> + "1" -> close level-1 code 1
  eng <- run_arb(micro_batch(
    sprintf('<RECODE> 2, Ind, "%s"', file.path(fx, "indtree.grc"))
  ))
  # codes now: total, 1, 2, 21, 99 (children 11/12 folded into their
  # level-1 parent; matches the Python reference engine byte-for-byte)
  expect_equal(val_at(eng, 1L, "", "1"), 28000)
  expect_equal(val_at(eng, 1L, "", "2"), 12000)
  expect_equal(val_at(eng, 1L, "", "21"), 12000)
  expect_equal(val_at(eng, 1L, " A", "1"), 19000)
  expect_equal(val_at(eng, 1L, " B", "1"), 9000)
  expect_equal(val_at(eng, 1L, " A", "21"), 2000)
  expect_equal(val_at(eng, 1L, " B", "21"), 10000)
  expect_error(val_at(eng, 1L, " A", "11"), "not found")
})

test_that("SPECIFYTABLE with an unknown variable fails", {
  expect_error(
    run_arb(c(
      sprintf('<OPENMICRODATA> "%s"', file.path(fx, "ex.asc")),
      sprintf('<OPENMETADATA> "%s"', file.path(fx, "ex.rda")),
      '<SPECIFYTABLE> "Nope""Reg"|"Amt"||',
      "<SAFETYRULE> NK(2,50)",
      "<READMICRODATA>",
      "<GOINTERACTIVE>"
    )),
    class = "BatchError"
  )
})
