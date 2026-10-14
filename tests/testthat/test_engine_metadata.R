# .rda metadata parser tests for the native engine driver.
# Pure R: no C-library calls are made here.

fx <- test_path("fixtures", "engine")

test_that("microdata .rda is parsed (positions, widths, missing, hierarchy, decimals)", {
  meta <- rtauargus.native:::rta_parse_rda(file.path(fx, "ex.rda"))

  expect_equal(length(meta$variables), 4)
  expect_equal(vapply(meta$variables, `[[`, character(1), "name"),
               c("Size", "Reg", "Ind", "Amt"))

  size <- meta$variables[[1]]
  expect_equal(size$b_pos, 1L)
  expect_equal(size$var_len, 3L)
  expect_equal(trimws(size$missing[1L]), "9")
  expect_false(nzchar(size$missing[2L]))
  expect_false(rtauargus.native:::rta_var_is_numeric(size))

  reg <- meta$variables[[2]]
  expect_equal(reg$b_pos, 4L)
  expect_equal(reg$var_len, 2L)
  expect_equal(trimws(reg$missing[1L]), "X")

  ind <- meta$variables[[3]]
  expect_equal(ind$b_pos, 6L)
  expect_equal(ind$var_len, 2L)
  expect_equal(ind$hierarchical, 1L) # <HIERLEVELS>
  expect_equal(ind$hier_levels, c(1L, 1L))

  amt <- meta$variables[[4]]
  expect_equal(amt$b_pos, 8L)
  expect_equal(amt$var_len, 8L)
  expect_equal(amt$n_decimals, 2L)
  expect_equal(trimws(amt$missing[1L]), "99999999")
  expect_true(rtauargus.native:::rta_var_is_numeric(amt))
})

test_that("table-input .rda is parsed (separator, tags, total codes)", {
  meta <- rtauargus.native:::rta_parse_rda_table(file.path(fx, "tab.rda"))

  expect_equal(meta$field_separator, ";")
  expect_equal(length(meta$variables), 11)
  expect_equal(vapply(meta$variables, `[[`, character(1), "name"),
               c("Size", "Region", "Var2", "FreqVar", "Var2_Shadow",
                 "Var2_Cost", "Top1", "Top2", "StatusVar",
                 "LowerProtLevel", "UpperProtLevel"))

  size <- meta$variables[[1]]
  expect_equal(size$tot_code, "Total")
  expect_equal(trimws(size$missing[1L]), "99")
  expect_false(rtauargus.native:::rta_var_is_numeric(size))

  resp <- meta$variables[[3]]
  expect_true(rtauargus.native:::rta_var_is_numeric(resp))
  expect_equal(resp$n_decimals, 2L)
  expect_equal(resp$type, "RESPONSE")

  by_type <- setNames(vapply(meta$variables, `[[`, character(1), "type"),
                      vapply(meta$variables, `[[`, character(1), "name"))
  expect_equal(unname(by_type["FreqVar"]), "FREQUENCY")
  expect_equal(unname(by_type["Var2_Shadow"]), "SHADOW")
  expect_equal(unname(by_type["Var2_Cost"]), "COST")
  expect_equal(unname(by_type["Top1"]), "TOP_N")
  expect_equal(unname(by_type["Top2"]), "TOP_N")
  expect_equal(unname(by_type["StatusVar"]), "STATUS")
  expect_equal(unname(by_type["LowerProtLevel"]), "LOWER_PROTECTION_LEVEL")
  expect_equal(unname(by_type["UpperProtLevel"]), "UPPER_PROTECTION_LEVEL")
})

test_that("status symbols map the CS_* codes", {
  rta_status_symbol <- rtauargus.native:::rta_status_symbol
  expect_equal(rta_status_symbol(rtauargus.native:::CS_SAFE), "S")
  expect_equal(rta_status_symbol(rtauargus.native:::CS_SAFE_MANUAL), "S")
  expect_equal(rta_status_symbol(rtauargus.native:::CS_UNSAFE_RULE), "U")
  expect_equal(rta_status_symbol(rtauargus.native:::CS_UNSAFE_MANUAL), "U")
  expect_equal(rta_status_symbol(rtauargus.native:::CS_SECONDARY_UNSAFE), "M")
  expect_equal(rta_status_symbol(rtauargus.native:::CS_EMPTY), "E")
  expect_equal(rta_status_symbol(rtauargus.native:::CS_EMPTY_NONSTRUCT), "E")
})

test_that("total-code and missing-code helpers behave like the legacy", {
  v <- rtauargus.native:::new_variable("A")
  v$tot_code <- "Total"
  expect_true(rtauargus.native:::rta_var_is_total_code(v, "TOTAL"))
  expect_false(rtauargus.native:::rta_var_is_total_code(v, " A "))
  expect_equal(rtauargus.native:::rta_var_get_total_code(v), "Total")

  v$missing <- c("99", "")
  expect_true(rtauargus.native:::rta_var_is_missing(v, "99"))
  expect_false(rtauargus.native:::rta_var_is_missing(v, "98"))
  expect_equal(rtauargus.native:::rta_var_n_missings(v), 1L)

  v2 <- rtauargus.native:::new_variable("B")
  v2$var_len <- 3L
  expect_equal(rtauargus.native:::rta_var_pad_code(v2, "7"), "  7")
})
