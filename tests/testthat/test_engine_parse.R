# Batch (.arb) parser and command-class tests for the native engine driver.
# Pure R: no C-library calls are made here.

# Writes a batch file into the (ephemeral) test tempdir and returns its path.
local_arb <- function(lines) {
  p <- tempfile(fileext = ".arb")
  writeLines(lines, p)
  p
}

test_that("parse_batch parses a full microdata batch into typed commands", {
  p <- local_arb(c(
    "// comment line",
    '<OPENMICRODATA> "data.asc"',
    '<OPENMETADATA> "data.rda"',
    '<SPECIFYTABLE> "A""B"|"Resp"|"Shadow"',
    "<SAFETYRULE> NK(2,50)|NK(0,0)",
    "<READMICRODATA>",
    "<RECODE> 1, A, \"a.grc\"",
    "<GOINTERACTIVE>"
  ))
  cmds <- rtauargus:::parse_batch(p)

  # 7 lines, but NK(2,50)|NK(0,0) yields two SafetyRule commands
  expect_length(cmds, 8)
  expect_s3_class(cmds[[1]], "rta_OpenMicrodata")
  expect_equal(cmds[[1]]$file, "data.asc") # unquoted
  expect_s3_class(cmds[[2]], "rta_OpenMetadata")
  expect_equal(cmds[[2]]$file, "data.rda")
  expect_s3_class(cmds[[3]], "rta_SpecifyTable")
  expect_equal(cmds[[3]]$exp_vars, c("A", "B"))
  expect_equal(cmds[[3]]$resp_var, "Resp")
  expect_equal(cmds[[3]]$shadow_var, "Shadow")
  # NK(2,50)|NK(0,0) -> two separate SafetyRule commands
  expect_s3_class(cmds[[4]], "rta_SafetyRule")
  expect_equal(cmds[[4]]$kind, "NK")
  expect_equal(cmds[[4]]$args, c("2", "50"))
  expect_s3_class(cmds[[5]], "rta_SafetyRule")
  expect_equal(cmds[[5]]$args, c("0", "0"))
  expect_s3_class(cmds[[6]], "rta_ReadMicrodata")
  expect_s3_class(cmds[[7]], "rta_Recode")
  expect_s3_class(cmds[[8]], "rta_GoInteractive")
  # every command carries the generic class too
  expect_true(all(vapply(cmds, inherits, logical(1), "rta_command")))
})

test_that("Suppress commands keep their kind field (rta_cmd regression)", {
  p <- local_arb(c(
    '<OPENMICRODATA> "d.asc"',
    '<OPENMETADATA> "d.rda"',
    '<SPECIFYTABLE> "A"|"R"|',
    "<SAFETYRULE> NK(2,50)",
    "<READMICRODATA>",
    "<SUPPRESS> MOD(1)",
    "<SUPPRESS> OPT(1)",
    "<SUPPRESS> RND(1, 10000)"
  ))
  cmds <- rtauargus:::parse_batch(p)
  suppresses <- cmds[6:8]

  # the class is rta_Suppress (not rta_MOD / rta_OPT / rta_RND)
  expect_true(all(vapply(suppresses, inherits, logical(1), "rta_Suppress")))
  expect_length(suppresses, 3)
  # and the method survives in the kind field
  expect_equal(vapply(suppresses, `[[`, character(1), "kind"),
               c("MOD", "OPT", "RND"))
  expect_equal(vapply(suppresses, `[[`, integer(1), "tab_no"),
               c(1L, 1L, 1L))
})

test_that("MOD option list is parsed into normalised fields", {
  p <- local_arb(c(
    '<OPENMICRODATA> "d.asc"',
    '<OPENMETADATA> "d.rda"',
    '<SPECIFYTABLE> "A"|"R"|',
    "<SAFETYRULE> NK(2,50)",
    "<READMICRODATA>",
    "<SUPPRESS> MOD(1, 60, 1, 0, 1, 0.1, 0.2, 0.3)"
  ))
  cmd <- rtauargus:::parse_batch(p)[[6]]
  expect_equal(cmd$kind, "MOD")
  expect_equal(cmd$mod_max_time, 60L)
  expect_true(cmd$mod_singleton)
  expect_false(cmd$mod_singleton_multi)
  expect_true(cmd$mod_min_freq)
  expect_equal(cmd$mod_msc, 0.1)
  expect_equal(cmd$mod_lower_marg, 0.2)
  expect_equal(cmd$mod_upper_marg, 0.3)
})

test_that("RND nested argument list is parsed (base, steps, time, partitions, stop rule, unit cost)", {
  p <- local_arb(c(
    '<OPENMICRODATA> "d.asc"',
    '<OPENMETADATA> "d.rda"',
    '<SPECIFYTABLE> "A"|"R"|',
    "<SAFETYRULE> NK(2,50)",
    "<READMICRODATA>",
    "<SUPPRESS> RND(1, 10000, 2, 5, 0, 1, 1)"
  ))
  cmd <- rtauargus:::parse_batch(p)[[6]]
  expect_equal(cmd$rnd_base, 10000L)
  expect_equal(cmd$rnd_step, 2L)
  expect_equal(cmd$rnd_time, 5L)
  expect_equal(cmd$rnd_partitions, 0L)
  expect_equal(cmd$rnd_stop_rule, 1L)
  expect_true(cmd$rnd_unit_cost)
  # defaults for an RND with only the mandatory base
  p2 <- local_arb(c(
    '<OPENMICRODATA> "d.asc"',
    '<OPENMETADATA> "d.rda"',
    '<SPECIFYTABLE> "A"|"R"|',
    "<SAFETYRULE> NK(2,50)",
    "<READMICRODATA>",
    "<SUPPRESS> RND(1, 100)"
  ))
  cmd2 <- rtauargus:::parse_batch(p2)[[6]]
  expect_equal(cmd2$rnd_base, 100L)
  expect_equal(cmd2$rnd_step, 0L)
  expect_equal(cmd2$rnd_time, 10L) # default
  expect_true(cmd2$rnd_unit_cost)
})

test_that("WRITETABLE grammar: options, empty-options form and type bounds", {
  p <- local_arb(c('<WRITETABLE> (1, 5, AR+, "out.tab")'))
  cmd <- rtauargus:::parse_batch(p)[[1]]
  expect_s3_class(cmd, "rta_WriteTable")
  expect_equal(cmd$tab_no, 1L)
  expect_equal(cmd$output_type, 5L)
  expect_equal(cmd$options, "+AR")
  expect_equal(cmd$file, "out.tab")

  # the legacy empty-options form uses a doubled comma
  p2 <- local_arb(c('<WRITETABLE> (2, 1,, "f.csv")'))
  cmd2 <- rtauargus:::parse_batch(p2)[[1]]
  expect_equal(cmd2$tab_no, 2L)
  expect_equal(cmd2$output_type, 1L)
  expect_length(cmd2$options, 0)
  expect_equal(cmd2$file, "f.csv")

  # output types are bounded 1..7
  p3 <- local_arb(c('<WRITETABLE> (1, 8, "f.csv")'))
  expect_error(rtauargus:::parse_batch(p3), class = "BatchError")
})

test_that("READTABLE flags are parsed", {
  # READTABLE is only allowed in table-input position (after the data,
  # metadata and table specification)
  p <- local_arb(c(
    '<OPENTABLEDATA> "t.tab"',
    '<OPENMETADATA> "t.rda"',
    '<SPECIFYTABLE> "A"|"R"|',
    "<READTABLE> 1T"
  ))
  cmd <- rtauargus:::parse_batch(p)[[4]]
  expect_s3_class(cmd, "rta_ReadTable")
  expect_equal(cmd$additivity, 1L)
  expect_true(cmd$keep_status)

  p2 <- local_arb(c(
    '<OPENTABLEDATA> "t.tab"',
    '<OPENMETADATA> "t.rda"',
    '<SPECIFYTABLE> "A"|"R"|',
    "<READTABLE> 0"
  ))
  cmd2 <- rtauargus:::parse_batch(p2)[[4]]
  expect_equal(cmd2$additivity, 0L)
  expect_false(cmd2$keep_status)
})

test_that("RECODE command fields are parsed", {
  p <- local_arb(c('<RECODE> 1, A, "a.grc"'))
  cmd <- rtauargus:::parse_batch(p)[[1]]
  expect_s3_class(cmd, "rta_Recode")
  expect_equal(cmd$tab_no, 1L)
  expect_equal(cmd$var_name, "A")
  expect_equal(cmd$recode_file, "a.grc") # unquoted
})

test_that("command ordering violations are rejected", {
  # OPENMETADATA before any data file
  p <- local_arb(c('<OPENMETADATA> "d.rda"'))
  expect_error(rtauargus:::parse_batch(p), class = "BatchError")

  # SUPPRESS directly after SPECIFYTABLE (before any SAFETYRULE)
  p2 <- local_arb(c(
    '<OPENMICRODATA> "d.asc"',
    '<OPENMETADATA> "d.rda"',
    '<SPECIFYTABLE> "A"|"R"|',
    "<SUPPRESS> MOD(1)"
  ))
  expect_error(rtauargus:::parse_batch(p2), class = "BatchError")

  # unknown keyword
  p3 <- local_arb(c('<NOTAKNOWN> x'))
  expect_error(rtauargus:::parse_batch(p3), class = "BatchError")
})

test_that("BatchError is signalled without warnings (stop() regression)", {
  # stop(cond, call. = FALSE) used to warn "additional arguments ignored";
  # the call is hidden via the condition's own call = NULL field instead.
  expect_no_warning(
    expect_error(rtauargus:::stop_batch("boom"), class = "BatchError")
  )
  expect_no_warning(
    expect_error(
      rtauargus:::parse_batch(local_arb(c('<NOTAKNOWN> x'))),
      class = "BatchError")
  )
})
