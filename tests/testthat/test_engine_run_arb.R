# End-to-end test for the public run_arb() driver: batches are executed
# in-process by the native engine (no TauArgus.exe, no system() call).
#
# Expected values were verified against the Python reference engine
# (pytauargus) running the same native libraries.
#
# Note: the WRITETABLE line uses the unspaced (n, type, options, "file")
# form so that the driver's own pre-flight parsing (write_decompose)
# recognises the output name.

fx <- test_path("fixtures", "engine")

test_that("run_arb() executes a microdata batch with the native engine", {
  skip_if(!file.exists(file.path(fx, "ex.asc")) ||
            !file.exists(file.path(fx, "ex.rda")),
          "engine fixtures not available")

  out_csv <- file.path(tempdir(), "run_arb_e2e.csv")
  arb <- tempfile(fileext = ".arb")
  writeLines(c(
    sprintf('<OPENMICRODATA> "%s"', file.path(fx, "ex.asc")),
    sprintf('<OPENMETADATA> "%s"', file.path(fx, "ex.rda")),
    '<SPECIFYTABLE> "Size""Reg"|"Amt"||',
    "<SAFETYRULE> NK(2,50)|NK(0,0)",
    "<READMICRODATA>",
    "<SUPPRESS> RND(1, 10000)",
    sprintf('<WRITETABLE> (1,1,,\"%s\")', out_csv),
    "<GOINTERACTIVE>"
  ), arb)

  res <- suppressMessages(run_arb(arb))

  # import = FALSE (default) -> nothing is returned
  expect_null(res)

  # the batch produced the CSV with the rounded values
  lines <- readLines(out_csv, warn = FALSE)
  # the writer terminates the file with a blank line; drop it
  lines <- lines[nzchar(lines)]
  expect_length(lines, 6)
  expect_identical(lines[1], ',"Total"," A"," B"," X"')
  expect_identical(lines[2], '"Total",40000.00,20000.00,20000.00,-')
  expect_identical(lines[3], '"  1",10000.00,0.00,10000.00,-')
  expect_identical(lines[4], '"  2",20000.00,10000.00,10000.00,-')
  expect_identical(lines[5], '"  3",10000.00,10000.00,0.00,-')
  expect_identical(lines[6], '"  9",-,-,-,-')
})

test_that("run_arb() completes without any TauArgus executable", {
  skip_if(!file.exists(file.path(fx, "ex.asc")) ||
            !file.exists(file.path(fx, "ex.rda")),
          "engine fixtures not available")

  out_csv <- file.path(tempdir(), "run_arb_no_exe.csv")
  arb <- tempfile(fileext = ".arb")
  writeLines(c(
    sprintf('<OPENMICRODATA> "%s"', file.path(fx, "ex.asc")),
    sprintf('<OPENMETADATA> "%s"', file.path(fx, "ex.rda")),
    '<SPECIFYTABLE> "Size""Reg"|"Amt"||',
    "<SAFETYRULE> NK(2,50)|NK(0,0)",
    "<READMICRODATA>",
    "<SUPPRESS> RND(1, 10000)",
    sprintf('<WRITETABLE> (1,1,,\"%s\")', out_csv),
    "<GOINTERACTIVE>"
  ), arb)

  # The batch must run to completion even when the executable path does
  # not exist (before the native flip this stopped with
  # "TauArgus.exe not found"). The legacy-only arguments are accepted
  # but unused, and no logbook file is produced.
  logbook <- file.path(tempdir(), "no_logbook.txt")
  res <- suppressMessages(run_arb(
    arb,
    tauargus_exe = "/definitely/not/here/TauArgus.exe",
    logbook = logbook,
    verbose = TRUE
  ))
  expect_null(res)
  expect_false(file.exists(logbook))

  lines <- readLines(out_csv, warn = FALSE)
  lines <- lines[nzchar(lines)]
  expect_identical(lines[2], '"Total",40000.00,20000.00,20000.00,-')
  expect_identical(lines[6], '"  9",-,-,-,-')
})

test_that("run_arb() keeps its pre-flight checks", {
  skip_if(!file.exists(file.path(fx, "ex.asc")) ||
            !file.exists(file.path(fx, "ex.rda")),
          "engine fixtures not available")

  # missing batch file
  expect_error(
    run_arb(tempfile(fileext = ".arb")),
    "Batch file cannot be found"
  )

  # missing metadata file
  arb <- tempfile(fileext = ".arb")
  writeLines(c(
    sprintf('<OPENMICRODATA> "%s"', file.path(fx, "ex.asc")),
    '<OPENMETADATA> "does_not_exist.rda"',
    '<SPECIFYTABLE> "Size""Reg"|"Amt"||',
    "<READMICRODATA>",
    "<GOINTERACTIVE>"
  ), arb)
  expect_error(
    run_arb(arb),
    "Metadata file cannot be found"
  )
})
