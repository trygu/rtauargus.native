# The complete ex1 .arb job through the public run_arb() driver,
# validated against the hand-derived expectations shared with the
# Python installed tests. The batch is self-contained: the suppression
# (OPT) and the table outputs (type-1 matrix and type-5 AR+ audit) are
# part of the batch, so the native engine runs its own protection audit
# on the actual result before writing (no audit logic is reimplemented
# here).
#
# Validation is invariant-based: exact cell keys and response values,
# the preserved manual primary suppression and adequate protection in
# the audit, and the output table consistent with the audit. The
# secondary suppression pattern itself is not pinned (alternative
# optima exist).

fx <- test_path("fixtures", "ex1")

parse_ex1_expected <- function() {
  lines <- readLines(file.path(fx, "ex1.expected"), warn = FALSE)
  cells <- list()
  primary <- NULL
  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0L || startsWith(line, "#")) next
    parts <- strsplit(line, "\\s+")[[1L]]
    key <- strsplit(parts[2L], ";")[[1L]]
    if (parts[1L] == "CELL") {
      cells[[paste(trimws(key), collapse = ";")]] <-
        list(response = as.numeric(parts[3L]), status = parts[4L])
    } else if (parts[1L] == "PRIMARY") {
      primary <- list(row = trimws(key[1L]), col = trimws(key[2L]),
                      response = as.numeric(parts[3L]),
                      protection = as.numeric(parts[4L]))
    }
  }
  stopifnot(length(cells) > 0L, !is.null(primary))
  list(cells = cells, primary = primary)
}

unquote_field <- function(x) trimws(gsub('^"|"$', "", x))

test_that("run_arb executes the ex1 table job with suppression and audit", {
  skip_if_not(dir.exists(fx), "ex1 fixture not available")
  exp <- parse_ex1_expected()

  out_dir <- tempfile("rta_installed_arb_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  # Stage a working copy: the job batch keeps the fixture's relative
  # paths, so the batch file must sit next to the data files.
  workdir <- file.path(out_dir, "work")
  dir.create(workdir)
  file.copy(list.files(fx, full.names = TRUE), workdir)

  arb <- file.path(workdir, "job.arb")
  writeLines(c(
    '<OPENTABLEDATA> "ex1.tab"',
    '<OPENMETADATA> "ex1.rda"',
    '<SPECIFYTABLE> "Sto""Reg"|"Var2"||',
    "<READTABLE> 1T",
    "<SUPPRESS> OPT(1)",
    # no spaces after the commas: the batch preflight parses these
    # with a strict regex
    '<WRITETABLE> (1,1,,"out.tab")',
    '<WRITETABLE> (1,5,AR+,"audit.tab")',
    "<GOINTERACTIVE>"
  ), arb)

  old_wd <- setwd(workdir)
  on.exit(setwd(old_wd), add = TRUE)
  suppressMessages(rtauargus.native::run_arb("job.arb"))
  expect_true(file.exists(file.path(workdir, "out.tab")))
  expect_true(file.exists(file.path(workdir, "audit.tab")))

  # ---- type-5 AR+ audit: one row per cell, 15 fields each ---------
  # (no SAFETYRULE in the batch, so no holding-list columns)
  audit_lines <- readLines(file.path(workdir, "audit.tab"), warn = FALSE)
  expect_equal(length(audit_lines), 16L)
  rows <- lapply(audit_lines,
                 function(l) strsplit(l, ";", fixed = TRUE)[[1L]])
  expect_true(all(vapply(rows, length, integer(1)) == 15L))

  # field layout: 1 row, 2 col, 3 value, 4 freq, 5 shadow, 6 cost,
  # 7 status, 8 lower, 9 upper, 10 rlower, 11 rupper, 12 width,
  # 13 p1, 14 p2, 15 p3 (the AR+ audit columns)
  audit <- list()
  for (r in rows) {
    audit[[paste(unquote_field(r[1L]), unquote_field(r[2L]), sep = ";")]] <- r
  }
  expect_equal(sort(names(audit)), sort(names(exp$cells)))
  for (nm in names(exp$cells)) {
    expect_equal(as.numeric(audit[[nm]][3L]), exp$cells[[nm]]$response,
                 label = paste("audit value of", nm))
  }

  pr <- exp$primary
  pr_row <- audit[[paste(pr$row, pr$col, sep = ";")]]
  expect_identical(pr_row[7L], "U")
  expect_equal(as.numeric(pr_row[3L]), pr$response)
  rlower <- as.numeric(pr_row[10L])
  rupper <- as.numeric(pr_row[11L])
  width <- as.numeric(pr_row[12L])
  p1 <- as.numeric(pr_row[13L])
  p2 <- as.numeric(pr_row[14L])
  expect_true(rlower <= pr$response - pr$protection + 1e-9,
              info = "the primary is not protected from below")
  expect_true(rupper >= pr$response + pr$protection - 1e-9,
              info = "the primary is not protected from above")
  expect_true(width >= 2 * pr$protection - 1e-9)
  # the realized protection must meet the required margin
  # (protection as a percentage of the response)
  expect_true(p1 >= pr$protection / pr$response * 100 - 1e-6)
  expect_true(p2 >= pr$protection / pr$response * 100 - 1e-6)

  # ---- invariants over all withheld and safe cells ----------------
  statuses <- vapply(rows, `[`, character(1), 7L)
  expect_equal(sum(statuses == "U"), 1L)
  expect_gt(sum(statuses == "M"), 0L)
  tol <- 1e-9
  for (r in rows) {
    st <- r[7L]
    key <- paste(unquote_field(r[1L]), unquote_field(r[2L]), sep = ";")
    if (st %in% c("U", "M")) {
      # a withheld cell is audited and strictly interior: its value
      # is not exactly recoverable from the published table
      value <- as.numeric(r[3L])
      lo <- as.numeric(r[10L]); hi <- as.numeric(r[11L])
      expect_gt(value - lo, tol, label = paste("lower bound of", key))
      expect_gt(hi - value, tol, label = paste("upper bound of", key))
      expect_gt(as.numeric(r[12L]), tol, label = paste("width of", key))
    } else if (identical(st, "S")) {
      expect_identical(as.numeric(r[13L]), 0)
      expect_identical(as.numeric(r[14L]), 0)
      expect_identical(as.numeric(r[15L]), 0)
    } else {
      skip(paste("unexpected status", st, "for", key))
    }
  }

  # ---- type-1 output: matrix layout, consistent with the audit ----
  out_lines <- readLines(file.path(workdir, "out.tab"), warn = FALSE)
  out_lines <- out_lines[nzchar(trimws(out_lines))] # trailing newline
  expect_equal(length(out_lines), 5L) # header + 4 row codes
  header <- strsplit(out_lines[1L], ",")[[1L]]
  col_codes <- unquote_field(header[-1L])
  all_cols <- unique(sub("^[^;]*;", "", names(exp$cells)))
  expect_setequal(col_codes[-1L], setdiff(all_cols, "Total"))
  expect_equal(col_codes[1L], "Total")

  got <- list()
  row_codes <- character(0L)
  for (i in seq(2L, length(out_lines))) {
    f <- strsplit(out_lines[i], ",")[[1L]]
    row_code <- unquote_field(f[1L])
    row_codes <- c(row_codes, row_code)
    for (j in seq_along(col_codes)) {
      got[[paste(row_code, col_codes[j], sep = ";")]] <- trimws(f[1L + j])
    }
  }
  expect_setequal(row_codes,
                  c("Total", unique(sub(";.*", "", names(exp$cells)))))
  expect_setequal(names(got), names(exp$cells))
  for (nm in names(exp$cells)) {
    st <- audit[[nm]][7L]
    if (identical(st, "S")) {
      # published cells carry their response value
      expect_equal(as.numeric(got[[nm]]), exp$cells[[nm]]$response,
                   label = paste("published value of", nm))
    } else {
      # withheld cells (U or M) are blanked in the output table
      expect_true(got[[nm]] %in% c("x", "-"),
                  info = paste("withheld cell", nm, "is not blanked"))
    }
  }
})
