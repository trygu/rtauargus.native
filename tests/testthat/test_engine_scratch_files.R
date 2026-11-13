# The solver writes its scratch files (CSPlogfile.txt, cspSCIP.*, HiTaS.log,
# FullJJ.log, JJUit.dat, ...) into the out dir the front-end passes to
# AHiTaS/FullJJ. That out dir must be a temp dir, never the process working
# directory, or every MOD/OPT solve pollutes the user's working area.
#
# Legacy tau-ARGUS pointed the solver at the OS temp dir
# (OptiSuppress.java: AHiTaS(..., getTempDir()+"/")) and deleted the
# previous run's leftovers. The native front-ends do better: they pass a
# fresh per-call temp dir and delete it when the call ends.

fx <- test_path("fixtures", "engine")
out_dir <- tempfile("rta_engine_test_")
dir.create(out_dir)
on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

# Every scratch file name the solver layer can produce (HiTaS PrepFile
# names plus the CSP PPCSPSetFileNames names).
scratch_names <- c(
  "CSPlogfile.txt", "cspSCIP.sol", "cspSCIP.heu", "cspSCIP.sta",
  "cspSCIP.bra", "HiTaS.log", "FullJJ.log", "JJUit.dat", "hierinfo.dat",
  "BT.dat", "BTTabs.dat", "InFeas.dat", "hitassec.txt", "XUit.dat",
  "Tricks.dat", "sdcnet.lp", "sdc.lp", "problem.lp"
)

run_arb <- function(lines) {
  arb <- tempfile(fileext = ".arb")
  on.exit(unlink(arb), add = TRUE)
  writeLines(lines, arb)
  rtauargus.native:::run_engine_batch(arb)
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

# Run the solve and assert no solver scratch file is left in the working
# directory. Stale leftovers from earlier runs are removed first, so the
# check is not masked by pre-existing files. Returns the solve result so
# callers can add more expectations.
expect_no_scratch_in_cwd <- function(expr) {
  stale <- file.path(getwd(), scratch_names[file.exists(file.path(getwd(), scratch_names))])
  file.remove(stale)
  out <- expr
  leaked <- intersect(list.files(getwd()), scratch_names)
  expect_true(
    length(leaked) == 0L,
    label = sprintf("solver scratch files leaked into the working directory: %s",
                    paste(leaked, collapse = ", "))
  )
  invisible(out)
}

test_that("MOD suppression keeps solver scratch out of the working directory", {
  expect_no_scratch_in_cwd(expect_message(
    run_arb(micro_batch(c(
      "<SUPPRESS> MOD(1)",
      sprintf('<WRITETABLE> (1, 5, AR+, "%s")', file.path(out_dir, "audit_mod.tab"))
    ))),
    "MOD suppress"
  ))
})

test_that("OPT suppression keeps solver scratch out of the working directory", {
  expect_no_scratch_in_cwd(expect_message(
    run_arb(micro_batch(c(
      "<SUPPRESS> OPT(1)",
      sprintf('<WRITETABLE> (1, 5, AR+, "%s")', file.path(out_dir, "audit_opt.tab"))
    ))),
    "OPT suppress"
  ))
})
