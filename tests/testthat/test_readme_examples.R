# The README's quick start and hand-written batch examples, pinned to the
# suite so the documented behaviour stays true. The status letters, the
# published columns and the hand-batch output are the contract; the
# secondary suppression pattern beyond the marked cells is not pinned
# (alternative optima exist).

act_size <- data.frame(
  ACTIVITY = c("01", "01", "01", "02", "02", "02",
               "06", "06", "06", "Total", "Total", "Total"),
  SIZE = c("tr1", "tr2", "Total", "tr1", "tr2", "Total",
           "tr1", "tr2", "Total", "tr1", "tr2", "Total"),
  VAL = c(100, 50, 150, 30, 20, 50, 60, 40, 100, 190, 110, 300),
  N_OBS = c(10, 5, 15, 2, 5, 7, 8, 6, 14, 20, 16, 36),
  MAX = c(20, 15, 20, 20, 10, 20, 16, 38, 38, 20, 38, 38)
)

run_readme_quick_start <- function(dir) {
  tab_rtauargus(
    act_size,
    dir_name = dir,
    files_name = "ex1",
    explanatory_vars = c("ACTIVITY", "SIZE"),
    safety_rules = "FREQ(3,10)|NK(1,85)",
    value = "VAL",
    freq = "N_OBS",
    maxscore = "MAX",
    totcode = c(ACTIVITY = "Total", SIZE = "Total"),
    verbose = FALSE
  )
}

test_that("the README quick start protects the documented cells", {
  dir <- file.path(tempdir(), "tauargus_readme_ex1")
  dir.create(dir, showWarnings = FALSE)

  ex1 <- run_readme_quick_start(dir)

  # Returns the input table with a Status column.
  expect_equal(nrow(ex1), 12L)
  expect_named(ex1, c("ACTIVITY", "SIZE", "VAL", "N_OBS", "MAX", "Status"))

  status <- setNames(ex1$Status, paste(ex1$ACTIVITY, ex1$SIZE, sep = ";"))
  expect_equal(status[["02;tr1"]], "A")  # frequency 2 < 3
  expect_equal(status[["06;tr2"]], "B")  # single contributor, 95% dominance
  expect_equal(status[["02;tr2"]], "D")  # secondary
  expect_equal(status[["06;tr1"]], "D")  # secondary
  expect_setequal(names(status)[status == "V"], c(
    "01;tr1", "01;tr2", "01;Total", "02;Total",
    "06;Total", "Total;Total", "Total;tr1", "Total;tr2"
  ))

  # The table input set and the published CSV land in dir_name
  # (no .asc or logbook for table input).
  expect_true(all(file.exists(file.path(dir, c(
    "ex1.tab", "ex1.rda", "ex1.arb", "ex1.csv")))))

  # The published CSV carries the status and dominance diagnostics.
  pub <- read.csv(file.path(dir, "ex1.csv"), check.names = FALSE)
  expect_named(pub, c("ACTIVITY", "SIZE", "VAL", "N_OBS", "Status", "Dom"))
  b <- pub[pub$ACTIVITY == "06" & pub$SIZE == "tr2", ]
  expect_equal(b$Status, "B")
  expect_equal(b$Dom, 95, tolerance = 1e-6)
})

test_that("a hand-written batch runs over the quick start inputs", {
  dir <- file.path(tempdir(), "tauargus_readme_hand")
  dir.create(dir, showWarnings = FALSE)
  invisible(run_readme_quick_start(dir))

  arb <- file.path(dir, "ex1_hand.arb")
  writeLines(c(
    sprintf('<OPENTABLEDATA> "%s"', file.path(dir, "ex1.tab")),
    sprintf('<OPENMETADATA> "%s"', file.path(dir, "ex1.rda")),
    '<SPECIFYTABLE> "ACTIVITY""SIZE"|"VAL"||',
    "<SAFETYRULE> FREQ(3,10)|NK(1,85)",
    "<READTABLE> 1",
    "<SUPPRESS> MOD(1,5,1,0,0)",
    sprintf('<WRITETABLE> (1,2,AS+,"%s")', file.path(dir, "ex1_hand.csv"))
  ), arb)

  # Runs silently and returns NULL by default.
  expect_null(run_arb(arb))

  # The pivot export (type 2, AS+) carries the status codes:
  # 1 valid, 3 dominance, 5 frequency, 11 secondary.
  hand <- read.csv(file.path(dir, "ex1_hand.csv"), check.names = FALSE)
  expect_named(hand, c("ACTIVITY", "SIZE", "VAL", "Status"))
  code <- function(a, s) hand$Status[hand$ACTIVITY == a & hand$SIZE == s]
  expect_equal(code("02", "tr1"), 5)
  expect_equal(code("06", "tr2"), 3)
  expect_equal(code("02", "tr2"), 11)
  expect_equal(code("06", "tr1"), 11)
  expect_equal(code("01", "tr1"), 1)

  # import = TRUE brings the written table(s) back into R.
  imp <- run_arb(arb, import = TRUE)
  expect_type(imp, "list")
  expect_length(imp, 1L)
  expect_s3_class(imp[[1L]], "data.frame")
})
