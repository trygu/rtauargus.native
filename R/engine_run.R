# ---------------------------------------------------------------------------
# Engine driver: executes parsed .arb batch commands against the native
# Tau-Argus engine (tauargusengine package).
#
# Faithful R port of the Engine class in pytauargus/engine.py. The engine
# object is an environment holding the native objects (TauArgus, HiTaSCtrl,
# RounderCtrl), the parsed state, and method closures.
# ---------------------------------------------------------------------------

new_engine <- function() {
  e <- new.env(parent = emptyenv())
  e$tau <- new_tauargus()
  e$hitas <- new_hitas()
  e$rounder <- new_rounder()
  e$metadata <- NULL
  e$data_file <- ""
  e$is_table <- FALSE
  e$tables <- list()            # list of list(spec, srs), 0-based table index
  e$safety_rules_buf <- list()  # per-table buffered SafetyRule commands
  e$n_tables <- 0L
  e$work_dir <- getwd()
  e$rounded_tables <- integer(0)
  e$ckm_tables <- integer(0)
  e$protect_cover_table <- FALSE
  e$audit_map <- list()         # tab (char) -> named list of c(min, max, unsafe)

  e$version <- function() tauargus_version(e$tau)

  # =========================================================================
  # Microdata flow
  # =========================================================================
  e$open_microdata <- function(data_file, meta) {
    e$data_file <- data_file
    e$metadata <- meta
    meta$data_file <- data_file

    tauargus_clean_all(e$tau)
    tauargus_set_in_file_info(e$tau, TRUE, "")
    tauargus_set_number_var(e$tau, length(meta$variables))

    for (var in meta$variables) e$e_set_variable(var)

    res <- tauargus_explore_file(e$tau, data_file)
    if (!isTRUE(res$ok))
      stop_batch("ExploreFile failed: ", tauargus_error_string(e$tau, res$err),
                 " line ", res$line)
  }

  e$e_set_variable <- function(var) {
    n_miss <- rta_var_n_missings(var)
    tauargus_set_variable(
      e$tau,
      idx = var$index,
      b_pos = var$b_pos,
      n_pos = var$var_len,
      n_dec = var$n_decimals,
      n_missing = n_miss,
      missing_1 = if (n_miss >= 1L) var$missing[1L] else "",
      missing_2 = if (n_miss >= 2L) var$missing[2L] else "",
      total_code = rta_var_get_total_code(var),
      is_peeper = rta_var_is_request(var),
      peeper_code_1 = if (rta_var_is_request(var)) var$request_code[1L] else "",
      peeper_code_2 = if (rta_var_is_request(var)) var$request_code[2L] else "",
      is_categorical = rta_var_is_categorical(var),
      is_numeric = rta_var_is_numeric(var),
      is_weight = rta_var_is_weight(var),
      is_hierarchical = var$hierarchical != 0L,
      is_holding = rta_var_is_holding(var),
      is_record_key = rta_var_is_record_key(var)
    )
    if (var$hierarchical == 2L) {
      # HIER_FILE
      hier_path <- e$e_resolve_path(var$hier_file_name)
      tauargus_set_hierarchical_codelist(e$tau, var$index,
                                         as.character(hier_path),
                                         var$leading_string)
    } else if (var$hierarchical == 1L) {
      # HIER_LEVELS — trim trailing zero levels (Java counts up to the last
      # non-zero).
      levels <- var$hier_levels
      n_levels <- 0L
      for (j in seq_along(levels))
        if (levels[j] != 0) n_levels <- j
      if (!isTRUE(tauargus_set_hierarchical_digits(
        e$tau, var$index,
        if (n_levels > 0L) levels[1L:n_levels] else numeric(0))))
        stop_batch("SetHierarchicalDigits failed for variable ", var$name,
                   " levels=", paste(levels[1L:n_levels], collapse = ","),
                   " var_len=", var$var_len)
    }
  }

  e$e_resolve_path <- function(filename) {
    # Resolve a file path relative to the data file's directory.
    p <- filename
    if (file.exists(p)) return(p)
    if (nzchar(e$data_file)) {
      p2 <- file.path(dirname(e$data_file), basename(p))
      if (file.exists(p2)) return(p2)
    }
    stop_batch("File not found: ", filename)
  }

  e$e_finalize_tables <- function() {
    e$n_tables <- length(e$tables)
    tauargus_set_number_tab(e$tau, e$n_tables)
    for (i in seq_along(e$tables)) {
      e$e_call_set_table(i - 1L, e$tables[[i]]$spec)
      if (isTRUE(e$is_table)) {
        # Table input: tell the engine which columns carry freq/maxscore/status
        # so primary rules are evaluated in CompletedTable.
        e$e_call_set_table_safety_info(i - 1L, e$tables[[i]]$srs)
      } else {
        e$e_call_set_table_safety(i - 1L, e$tables[[i]]$srs)
      }
    }
  }

  e$e_call_set_table <- function(idx, spec) {
    if (is.null(e$metadata)) stop_batch("No metadata.")
    index_of <- function(name) meta_index_of(e$metadata, name)
    exp_indices <- vapply(spec$exp_vars, index_of, integer(1))
    resp_idx <- index_of(spec$resp_var)
    resp_var <- e$metadata$variables[[resp_idx + 1L]]
    is_freq <- resp_var$type == "FREQUENCY"
    # Shadow: native rejects -1 for a non-frequency table, so default to the
    # response variable (Java: indexOfShadowVariable() falls back to resp).
    shadow_idx <- if (is_freq) -1L else
      if (nzchar(spec$shadow_var)) index_of(spec$shadow_var) else resp_idx
    # Legacy default: omitted cost means the response variable;
    # explicit special codes -1/-2/-3 map to the native sentinels.
    if (!nzchar(spec$cost_var)) {
      cost_idx <- resp_idx
    } else if (spec$cost_var == "-1") {
      cost_idx <- -1L
    } else if (spec$cost_var == "-2") {
      cost_idx <- -2L
    } else if (spec$cost_var == "-3") {
      cost_idx <- -3L
    } else {
      cost_idx <- index_of(spec$cost_var)
    }

    req_idx <- index_of("REQUEST")

    ok <- tauargus_set_table(
      e$tau,
      index = idx,
      explanatory = exp_indices,
      is_freq = is_freq,
      resp = resp_idx,
      shadow = shadow_idx,
      cost = cost_idx,
      cellkey = index_of("RECORD_KEY"),
      ckm_type = resp_var$ckm_type,
      ckm_topk = resp_var$ckm_top_k,
      lam = spec$lam,
      max_scaled_cost = 20000,
      peep_var = req_idx,
      missing_as_safe = FALSE
    )
    if (!isTRUE(ok)) stop_batch("SetTable failed for table ", idx + 1L)
  }

  e$e_call_set_table_safety <- function(idx, srs) {
    # Pad freq arrays to at least 2 entries (native expects >= 2)
    min_freq <- as.numeric(srs$min_freq)
    freq_marge <- as.numeric(srs$freq_marge)
    while (length(min_freq) < 2) min_freq <- c(min_freq, 0)
    while (length(freq_marge) < 2) freq_marge <- c(freq_marge, 0)
    # fix min_freq 0 -> 1 (as Java does)
    min_freq <- ifelse(min_freq > 0, min_freq, 1)
    # if freq_marge[0] < freq_marge[1] and min_freq[1] > 0: freq_marge[0] = ...
    if (freq_marge[1L] < freq_marge[2L] && min_freq[2L] > 0)
      freq_marge[1L] <- freq_marge[2L]

    ok <- tauargus_set_table_safety(
      e$tau,
      index = idx,
      dom = srs$dom_rule,
      dom_n = as.numeric(srs$dom_n),
      dom_k = as.numeric(srs$dom_k),
      pq = srs$pq_rule,
      pq_p = as.numeric(srs$pq_p),
      pq_q = as.numeric(srs$pq_q),
      pq_n = as.numeric(srs$pq_n),
      min_freq = min_freq,
      peep_perc = as.numeric(srs$peep_percentage),
      peep_marge = as.numeric(srs$peep_marge),
      peep_min_freq = as.numeric(srs$peep_min_freq),
      apply_peep = srs$apply_peep,
      apply_weight = srs$apply_weight,
      weight_on_safety = srs$weight_on_safety,
      apply_holding = srs$apply_holding,
      zero_rule = srs$zero_rule,
      empty_as_ns = FALSE,
      ns_range = 10,
      zero_range = srs$zero_range,
      manual_perc = srs$manual_perc,
      freq_perc = freq_marge
    )
    if (!isTRUE(ok)) stop_batch("SetTableSafety failed for table ", idx + 1L)
  }

  e$e_call_set_table_safety_info <- function(idx, srs) {
    # Port of the Java/Python SetTableSafetyInfo call for a tabular table.
    # It tells the engine which variable types are present so primary
    # safety rules (frequency, dominance) are applied.
    meta <- e$metadata
    has_max_score <- meta_contains(meta, "TOP_N")
    dom_n <- as.numeric(srs$dom_n)
    dom_p <- as.numeric(srs$dom_k)
    while (length(dom_n) < 4) { dom_n <- c(dom_n, 0); dom_p <- c(dom_p, 0) }
    pq_p <- as.numeric(srs$pq_p)
    pq_q <- as.numeric(srs$pq_q)
    pq_n <- as.numeric(srs$pq_n)
    while (length(pq_p) < 4) { pq_p <- c(pq_p, 0); pq_q <- c(pq_q, 0); pq_n <- c(pq_n, 0) }
    has_freq <- length(srs$min_freq) > 0
    freq_perc <- if (length(srs$freq_marge) > 0) as.integer(srs$freq_marge[1L]) else 0L
    safe_min_rec <- if (length(srs$min_freq) > 0) as.integer(srs$min_freq[1L]) else 1L
    has_status <- !(srs$dom_rule || srs$pq_rule || has_freq ||
                      srs$zero_rule || srs$apply_weight || srs$apply_peep)

    ok <- tauargus_set_table_safety_info(
      e$tau,
      index = idx,
      has_maxscore = has_max_score,
      dom = srs$dom_rule,
      dom_n = dom_n,
      dom_p = dom_p,
      pq = srs$pq_rule,
      pq_p = pq_p,
      pq_q = pq_q,
      pq_n = pq_n,
      has_freq = has_freq,
      freq_perc = freq_perc,
      safe_min_rec = safe_min_rec,
      has_status = has_status,
      manual_perc = as.numeric(srs$manual_perc),
      zero_rule = srs$zero_rule,
      zero_range = as.numeric(srs$zero_range),
      empty_as_ns = FALSE,
      ns_range = 10L
    )
    if (!isTRUE(ok)) stop_batch("SetTableSafetyInfo failed for table ", idx + 1L)
  }

  e$read_microdata <- function() {
    # Trigger table computation from microdata.
    e$e_finalize_tables()
    res <- tauargus_compute_tables(e$tau)
    if (!isTRUE(res$ok))
      stop_batch("ComputeTables failed: ",
                 tauargus_error_string(e$tau, res$err),
                 " table ", res$table_index)
    message("Computed ", e$n_tables, " tables.")
  }

  # =========================================================================
  # Table-input (tabular data) flow
  # =========================================================================
  e$open_table_data <- function(data_file, meta) {
    e$data_file <- data_file
    e$metadata <- meta
    meta$data_file <- data_file
    e$is_table <- TRUE
  }

  e$e_init_table_engine <- function(meta) {
    # Native setup for the tabular flow (port of TableService.readTables).
    tauargus_clean_all(e$tau)
    tauargus_set_in_file_info(e$tau, FALSE, meta$field_separator)
    tauargus_set_number_var(e$tau, length(meta$variables))

    e$e_compute_var_lengths(meta)

    for (var in meta$variables) e$e_set_variable(var)

    # Mark that tables are given directly rather than computed from micro
    # data (Java: ThroughTable -> m_UsingMicroData = false).
    tauargus_through_table(e$tau)
    tauargus_set_number_tab(e$tau, length(e$tables))
  }

  e$read_table <- function(additivity = 0L, keep_status = FALSE) {
    # Read a tabular table: port of TableSet.read (3-phase flow).
    meta <- e$metadata
    if (is.null(meta)) stop_batch("No metadata.")
    if (!e$is_table) stop_batch("read_table requires open_table_data first.")
    e$e_init_table_engine(meta)
    sep <- meta$field_separator
    if (!file.exists(e$data_file))
      stop_batch("Table data file not found: ", e$data_file)
    lines <- readLines(e$data_file, warn = FALSE, encoding = "UTF-8")

    for (i in seq_along(e$tables)) {
      spec <- e$tables[[i]]$spec
      srs <- e$tables[[i]]$srs
      exp_idx <- vapply(spec$exp_vars, function(n) meta_index_of(meta, n),
                        integer(1))
      if (any(exp_idx < 0L))
        stop_batch("Table ", i, ": explanatory variable not in metadata.")

      # -- Phase 1: register categorical codes -----------------------------
      for (line in lines) {
        if (!nzchar(trimws(line))) next
        fields <- rta_split_fields(line, sep)
        codes <- character(0)
        skip <- FALSE
        for (j in seq_along(meta$variables)) {
          var <- meta$variables[[j]]
          value <- if (j <= length(fields)) fields[j] else ""
          if (var$type == "CATEGORICAL") {
            if (rta_var_is_total_code(var, value)) {
              skip <- TRUE  # total rows are not codes
              break
            }
            codes <- c(codes, rta_var_normalise_code(var, value))
            if (rta_var_is_missing(var, value)) {
              skip <- TRUE  # missing rows are not codes (Java isMissing)
              break
            }
          }
        }
        if (skip) next
        if (length(codes) != length(exp_idx)) next
        e$e_set_in_code_list(exp_idx, codes)
      }

      res <- tauargus_set_totals_in_code_list(e$tau, exp_idx)
      if (!isTRUE(res$ok))
        stop_batch("SetTotalsInCodeList failed for table ", i, ": ",
                   tauargus_error_string(e$tau, res$err))

      # -- table + safety ---------------------------------------------------
      e$e_call_set_table(i - 1L, spec)
      e$e_call_set_table_safety_info(i - 1L, srs)

      # -- Phase 3: fill the cells ------------------------------------------
      has_top <- meta_contains(meta, "TOP_N")
      manual_marge <- srs$manual_perc
      for (line in lines) {
        if (!nzchar(trimws(line))) next
        fields <- rta_split_fields(line, sep)
        cell <- e$e_build_cell(fields, meta, has_top, manual_marge)
        if (is.null(cell)) next
        e$e_set_in_table(i - 1L, cell)
      }

      # -- complete the table ------------------------------------------------
      # Legacy: computeTotals = (additivity == ADDITIVITY_RECOMPUTE).
      # A cover table (<COVER>) has additivity NOT_REQUIRED, so its marginals
      # are not recomputed and the additivity check is skipped.
      compute_totals <- additivity == ADDITIVITY_RECOMPUTE
      set_totals_safe <-
        !(length(srs$freq_marge) > 0L || srs$dom_rule || srs$pq_rule)
      status_backup <- if (compute_totals && keep_status)
        e$e_backup_cell_statuses(i - 1L) else NULL
      res <- tauargus_completed_table(
        e$tau, idx = i - 1L, file = "", calc_totals = compute_totals,
        calc_totals_safe = set_totals_safe,
        for_cover = e$protect_cover_table
      )
      if (!isTRUE(res$ok))
        stop_batch("CompletedTable failed for table ", i, ": ",
                   tauargus_error_string(e$tau, res$err))
      if (!is.null(status_backup))
        e$e_restore_cell_statuses(i - 1L, status_backup)

      e$n_tables <- length(e$tables)
    }
  }

  e$e_compute_var_lengths <- function(meta) {
    # Set var_len from the data file (port of setVariableLengthFromData).
    for (i in seq_along(meta$variables))
      meta$variables[[i]]$var_len <- 0L
    path <- e$data_file
    if (!file.exists(path)) return(invisible())
    sep <- meta$field_separator
    for (line in readLines(path, warn = FALSE, encoding = "UTF-8")) {
      line <- trimws(line)
      if (!nzchar(line)) next
      fields <- rta_split_fields(line, sep)
      for (j in seq_along(meta$variables)) {
        if (j > length(fields)) break
        var <- meta$variables[[j]]
        if (rta_var_is_categorical(var) && var$hierarchical == 2L)
          next  # HIER_FILE: length comes from the hierarchy file
        if (fields[j] == var$tot_code) next
        if (nchar(fields[j]) > var$var_len) {
          var$var_len <- nchar(fields[j])
          meta$variables[[j]] <- var
        }
      }
    }
    # HIER_FILE variables take their length from the hierarchy file.
    for (i in seq_along(meta$variables)) {
      var <- meta$variables[[i]]
      if (rta_var_is_categorical(var) && var$hierarchical == 2L &&
          nzchar(var$hier_file_name)) {
        hier_path <- tryCatch(e$e_resolve_path(var$hier_file_name),
                              error = function(cond) NULL)
        if (is.null(hier_path)) next
        lead <- var$leading_string
        for (hline in readLines(hier_path, warn = FALSE, encoding = "UTF-8")) {
          hline <- trimws(hline)
          while (startsWith(hline, lead))
            hline <- trimws(substring(hline, nchar(lead) + 1L),
                            which = "left")
          if (nchar(hline) > var$var_len) {
            var$var_len <- nchar(hline)
            meta$variables[[i]] <- var
          }
        }
      }
    }
  }

  e$e_build_cell <- function(fields, meta, has_top, manual_marge) {
    # Port of TableSet.buildCell. Returns a list or NULL to skip.
    response <- CELL_UNKNOWN
    shadow <- CELL_UNKNOWN
    cost <- CELL_UNKNOWN
    lower <- CELL_UNKNOWN
    upper <- CELL_UNKNOWN
    freq <- CELL_UNKNOWN
    status <- CS_SAFE  # Java CellStatus.SAFE default
    max_score <- numeric(0)
    has_status <- FALSE
    codes <- character(0)

    for (j in seq_along(meta$variables)) {
      var <- meta$variables[[j]]
      value <- if (j <= length(fields)) fields[j] else ""
      t <- var$type
      if (t == "CATEGORICAL") {
        # isTotalCode2: trimmed equals totCode OR empty -> "" (total)
        vs <- trimws(value)
        if (vs == "" || vs == var$tot_code)
          codes <- c(codes, "")
        else
          codes <- c(codes, rta_var_normalise_code(var, value))
      } else if (t == "RESPONSE") {
        if (value %in% c("", "-")) status <- CS_EMPTY
        else response <- rta_to_double(value)
      } else if (t == "SHADOW") {
        if (!(value %in% c("", "-"))) shadow <- rta_to_double(value)
      } else if (t == "COST") {
        if (!(value %in% c("", "-"))) cost <- rta_to_double(value)
      } else if (t == "FREQUENCY") {
        if (value %in% c("", "-")) status <- CS_EMPTY
        else freq <- as.integer(rta_to_double(value))
      } else if (t == "TOP_N") {
        if (value %in% c("", "-")) max_score <- c(max_score, 0.0)
        else max_score <- c(max_score, rta_to_double(value))
      } else if (t == "LOWER_PROTECTION_LEVEL") {
        if (!(value %in% c("", "-"))) lower <- rta_to_double(value)
      } else if (t == "UPPER_PROTECTION_LEVEL") {
        if (!(value %in% c("", "-"))) upper <- rta_to_double(value)
      } else if (t == "STATUS") {
        has_status <- TRUE
        v <- toupper(value)
        if (v == "") status <- CS_EMPTY
        else if (v == "E") status <- CS_EMPTY
        else if (v == meta$safe_status) status <- CS_SAFE_MANUAL
        else if (v == meta$unsafe_status) status <- CS_UNSAFE_MANUAL
        else if (v == meta$protect_status) status <- CS_PROTECT_MANUAL
        else if (v == "M") status <- CS_SECONDARY_UNSAFE
        else stop_batch("Unknown status(", value, ")")
      }
    }

    # Consistency checks (port of buildCell checks 1-7)
    if (response == CELL_UNKNOWN && freq == CELL_UNKNOWN) {
      if (status %in% c(CS_SAFE, CS_SAFE_MANUAL)) status <- CS_EMPTY
      if (status != CS_EMPTY)
        stop_batch("An empty cell cannot have a status different from empty")
    }
    if (response == CELL_UNKNOWN && freq != CELL_UNKNOWN)
      stop_batch("An empty cell cannot have a real frequency")
    if (response != CELL_UNKNOWN && response != 0 && freq == 0)
      stop_batch("A real cell cannot have a frequency zero")
    if (response != CELL_UNKNOWN && freq == CELL_UNKNOWN) freq <- 1
    if (response == CELL_UNKNOWN && freq == CELL_UNKNOWN) status <- CS_EMPTY
    if (response == CELL_UNKNOWN && has_status && status != CS_EMPTY)
      status <- CS_EMPTY
    if (response != CELL_UNKNOWN && response != 0 &&
        freq != CELL_UNKNOWN && freq != 0 && status == CS_EMPTY)
      stop_batch("A non-empty cell cannot have a status empty")

    # Manual-unsafe protection levels
    if (status == CS_UNSAFE_MANUAL) {
      if (lower == CELL_UNKNOWN) {
        lower <- abs(response * manual_marge / 100)
        if (lower > response) lower <- response
      }
      if (upper == CELL_UNKNOWN) {
        upper <- abs(response * manual_marge / 100)
        if (upper > response) upper <- response
      }
    } else {
      lower <- 0
      upper <- 0
    }

    if (shadow == CELL_UNKNOWN) shadow <- response
    if (cost == CELL_UNKNOWN) cost <- abs(response)
    if (cost == 0) cost <- 0.0001
    if (cost < 0) stop_batch("Negative cost value found")

    # TopN must not exceed the total (and must be in non-increasing order).
    if (length(max_score) > 0L) {
      x <- sum(max_score)
      if (x > response + 1e-9)
        stop_batch("Sum of topN ", x, " should not exceed the cell total ",
                   response)
      if (x > response) max_score[1L] <- max_score[1L] - 1e-9
      # 2L:length(x) is c(2, 1) when length is 1; guard the loop.
      if (length(max_score) >= 2L)
        for (j in 2L:length(max_score))
          if (max_score[j] > max_score[j - 1L])
            stop_batch("Error in the order of the TopN.\n", j - 1L, " = ",
                       max_score[j - 1L], "\n", j, " = ", max_score[j], "\n",
                       "This is not allowed.")
    }

    if (status == CS_EMPTY || (freq == 0 && response == 0))
      return(NULL)  # empty / all-zero cells are not submitted

    list(codes = codes, shadow = shadow, cost = cost, response = response,
         freq = freq, max_score = max_score, status = status,
         lower = lower, upper = upper)
  }

  e$e_set_in_code_list <- function(exp_idx, codes) {
    # Register codes, retrying with padCode on a hierarchical miss.
    cur <- codes
    for (attempt in seq_len(length(exp_idx))) {
      res <- tauargus_set_in_code_list(e$tau, exp_idx, cur)
      if (isTRUE(res$ok)) return(invisible())
      if (res$err != ERR_CODENOTINCODELIST)
        stop_batch("SetInCodeList failed: ",
                   tauargus_error_string(e$tau, res$err),
                   " codes=", paste(cur, collapse = ","))
      var <- e$metadata$variables[[exp_idx[as.integer(res$ev) + 1L] + 1L]]
      cur[as.integer(res$ev) + 1L] <-
        rta_var_pad_code(var, cur[as.integer(res$ev) + 1L])
    }
    stop_batch("SetInCodeList failed: code not in code list codes=",
               paste(codes, collapse = ","))
  }

  e$e_set_in_table <- function(idx, cell) {
    # Fill one cell (codes already normalised by _build_cell).
    ok <- tauargus_set_in_table(
      e$tau, idx = idx, codes = cell$codes, shadow = cell$shadow,
      cost = cell$cost, resp = cell$response, freq = cell$freq,
      maxscore = cell$max_score, maxscore_hold = numeric(0),
      status = cell$status, lpl = cell$lower, upl = cell$upper
    )
    if (!isTRUE(ok))
      stop_batch("SetInTable failed for table ", idx + 1L,
                 ": codes=", paste(cell$codes, collapse = ","),
                 " err=", ok[1L], " ev=", ok[2L])
  }

  e$e_backup_cell_statuses <- function(idx) {
    sz <- tauargus_get_total_table_size(e$tau, idx)
    ncell <- as.integer(sz[1L])
    statuses <- integer(ncell)
    lpls <- numeric(ncell)
    upls <- numeric(ncell)
    for (nc in seq_len(ncell)) {
      statuses[nc] <- as.integer(
        tauargus_get_table_cell_status(e$tau, idx, nc - 1L))
      pl <- tauargus_get_table_cell_protection_levels(e$tau, idx, nc - 1L)
      lpls[nc] <- pl[1L]
      upls[nc] <- pl[2L]
    }
    list(statuses = statuses, lpls = lpls, upls = upls)
  }

  e$e_restore_cell_statuses <- function(idx, backup) {
    for (nc in seq_along(backup$statuses)) {
      st <- backup$statuses[nc]
      if (st %in% c(CS_UNKNOWN, CS_EMPTY, 13L)) next  # UNKNOWN/EMPTY/EMPTY_NONSTRUCT
      tauargus_set_table_cell_status_cell(e$tau, idx, nc - 1L, st)
      tauargus_set_table_cell_protection_levels(
        e$tau, idx, nc - 1L, backup$lpls[nc], backup$upls[nc])
    }
  }

  e$e_call_set_table_safety_info <- function(idx, srs) {
    # Port of the Java SetTableSafetyInfo call for a tabular table.
    has_max_score <- meta_contains(e$metadata, "TOP_N")
    dom_n <- as.numeric(srs$dom_n)
    dom_k <- as.numeric(srs$dom_k)
    while (length(dom_n) < 4) {
      dom_n <- c(dom_n, 0)
      dom_k <- c(dom_k, 0)
    }
    # Java passes domN as DominanceNumber and domK as DominancePerc.
    pq_p <- as.numeric(srs$pq_p)
    pq_q <- as.numeric(srs$pq_q)
    pq_n <- as.numeric(srs$pq_n)
    while (length(pq_p) < 4) {
      pq_p <- c(pq_p, 0)
      pq_q <- c(pq_q, 0)
      pq_n <- c(pq_n, 0)
    }
    has_freq <- length(srs$min_freq) > 0L
    freq_perc <- if (length(srs$freq_marge) > 0L) srs$freq_marge[1L] else 0
    safe_min_rec <- if (length(srs$min_freq) > 0L) srs$min_freq[1L] else 1
    # useStatusOnly: no (non-MAN) safety rule -> status is used directly.
    has_status <- !(srs$dom_rule || srs$pq_rule ||
                    length(srs$min_freq) > 0L ||
                    srs$zero_rule || srs$apply_weight || srs$apply_peep)
    ok <- tauargus_set_table_safety_info(
      e$tau,
      index = idx,
      has_maxscore = has_max_score,
      dom = srs$dom_rule,
      dom_n = dom_n,
      dom_p = dom_k,
      pq = srs$pq_rule,
      pq_p = pq_p, pq_q = pq_q, pq_n = pq_n,
      has_freq = has_freq,
      freq_perc = freq_perc,
      safe_min_rec = safe_min_rec,
      has_status = has_status,
      manual_perc = srs$manual_perc,
      zero_rule = srs$zero_rule,
      zero_range = srs$zero_range,
      empty_as_ns = FALSE,
      ns_range = 10
    )
    if (!isTRUE(ok)) stop_batch("SetTableSafetyInfo failed for table ",
                                idx + 1L)
  }

  # =========================================================================
  # Suppress (secondary suppression)
  # =========================================================================
  e$suppress <- function(cmd) {
    # Run suppression on the given table.
    tab <- as.integer(cmd$tab_no) - 1L
    kind <- toupper(cmd$kind)

    if (kind == "MOD") e$e_suppress_mod(tab, cmd)
    else if (kind == "OPT") e$e_suppress_opt(tab, cmd)
    else if (kind == "RND") e$e_suppress_rnd(tab, cmd)
    else if (kind == "CKM") e$e_suppress_ckm(tab, cmd)
    else stop_batch("Suppress method '", kind,
                    "' not supported in headless mode ",
                    "(GH/NET/CTA require external executables).")
  }

  e$e_table_cell_bounds <- function(tab) {
    # Return c(min, max) finite cell value of tab (Java minTabVal/maxTabVal).
    sz <- tauargus_get_total_table_size(e$tau, tab)
    n <- as.integer(sz[1L])
    lo <- NA_real_
    hi <- NA_real_
    for (i in seq_len(n)) {
      v <- tauargus_get_table_cell_value(e$tau, tab, i - 1L)
      if (is.na(v)) next
      if (is.na(lo) || v < lo) lo <- v
      if (is.na(hi) || v > hi) hi <- v
    }
    c(if (is.na(lo)) 0.0 else lo, if (is.na(hi)) 0.0 else hi)
  }

  e$e_min_round_base <- function(tab) {
    # Minimum legal rounding base (Java TableSet.computeMinRoundBase).
    b <- e$e_table_cell_bounds(tab)
    cands <- numeric(0)
    for (v in b) {
      d <- 0
      t <- as.numeric(v)
      while (t > 1) {
        t <- t / 10.0
        d <- d + 1L
      }
      if (d >= 1) cands <- c(cands, 10^(d - 1L))
    }
    if (length(cands) > 0L) as.integer(max(cands)) else 0L
  }

  e$e_suppress_mod <- function(tab, cmd) {
    tmp <- tempfile("rta_mod_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    param_file <- file.path(tmp, "NPF.txt")
    files_file <- file.path(tmp, "NFS.txt")
    # PrepareHITAS writes the parameter + file-list for the solver.
    if (!isTRUE(tauargus_prepare_hitas(e$tau, tab, param_file, files_file,
                                       tmp)))
      stop_batch("PrepareHITAS failed")
    ok <- hitas_a_hitas(
      e$hitas,
      pars_file = param_file,
      files_file = files_file,
      max_time = cmd$mod_max_time,
      single_with_single = cmd$mod_singleton,
      single_with_more = cmd$mod_singleton_multi,
      do_count_bounds = cmd$mod_min_freq
    )
    if (ok > 0)
      stop_batch("AHiTaS failed: ", hitas_error_string(e$hitas, ok))
    res <- tauargus_set_secondary_hitas(e$tau, tab)
    message("MOD suppress: ", res$n_set_secondary, " cells set secondary.")
  }

  e$e_suppress_opt <- function(tab, cmd) {
    tmp <- tempfile("rta_opt_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    jj_in <- file.path(tmp, "JJ.IN")
    jj_out <- file.path(tmp, "JJ.OUT")
    jj2 <- file.path(tmp, "JJ2.OUT")
    b <- e$e_table_cell_bounds(tab)
    if (!isTRUE(tauargus_write_jj_format(e$tau, tab, jj_in, b[1L], b[2L],
                                         FALSE, FALSE, FALSE)))
      stop_batch("WriteJJFormat failed for OPT")
    res <- hitas_full_jj(e$hitas, in_file_jj = jj_in, out_file = jj_out,
                         max_time = cmd$opt_max_time)
    if (res > 1)
      stop_batch("FullJJ failed (code ", res, "): ",
                 hitas_error_string(e$hitas, res))
    # Append " m" to every line of JJ.OUT (secondary-marker form) -> JJ2.OUT
    lines <- readLines(jj_out, warn = FALSE, encoding = "UTF-8")
    writeLines(c("fop", "fop", paste0(lines, " m")), jj2)
    res2 <- tauargus_set_secondary_jjformat(e$tau, tab, jj2, FALSE)
    message("OPT suppress: ", res2$n_set_secondary, " cells set secondary.")
  }

  e$e_suppress_rnd <- function(tab, cmd) {
    base <- as.integer(cmd$rnd_base)
    min_base <- e$e_min_round_base(tab)
    if (base < min_base)
      stop_batch("Rounding base ", base, " too small; minimum ", min_base,
                 " required")
    tmp <- tempfile("rta_rnd_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    jj_in <- file.path(tmp, "JJ.IN")
    jj_out <- file.path(tmp, "JJ.OUT")
    jj_stat <- file.path(tmp, "JJstat.OUT")
    b <- e$e_table_cell_bounds(tab)
    if (!isTRUE(tauargus_write_jj_format(e$tau, tab, jj_in, b[1L], b[2L],
                                         FALSE, FALSE, TRUE)))
      stop_batch("WriteJJFormat failed for RND")
    rta_correct_round_jj(jj_in, base, cmd$rnd_partitions == 0L,
                         cmd$rnd_unit_cost)
    # CRP/HiGHS constants (registry defaults).
    rounder_set_double_constant(e$rounder, 101, 0.0000001)   # JJZERO
    rounder_set_double_constant(e$rounder, 102, 21400000000000.0)  # JJINF
    rounder_set_double_constant(e$rounder, 103, 0.0001)      # JJMINVIOLA
    rounder_set_double_constant(e$rounder, 104, 0.01)        # JJMAXSLACK
    res <- rounder_do_round(
      e$rounder, solver = "SCIP", in_file = jj_in,
      base = as.numeric(base),
      upper_bound = 1.0e40, lower_bound = 0.0,
      solution_file = jj_out, statistics_file = jj_stat,
      max_time = cmd$rnd_time
    )
    if (res[[1L]] > 0)
      stop_batch("DoRound failed (result ", res[[1L]],
                 ", err ", res[[5L]], ")")
    tauargus_set_rounded_response(e$tau, jj_out, tab)
    e$rounded_tables <- c(e$rounded_tables, tab)
    message("RND suppress: table ", tab + 1L, " rounded (max_jump=",
            format(res[[2L]], digits = 4), ", ", res[[3L]], " steps).")
  }

  e$e_suppress_ckm <- function(tab, cmd) {
    if (nzchar(cmd$ckm_p_table)) {
      res <- tauargus_set_cell_key_values_freq(e$tau, tab, cmd$ckm_p_table)
      message("CKM freq: min=", res$min, " max=", res$max)
    } else if (nzchar(cmd$ckm_p_table_cont)) {
      if (is.null(e$metadata)) stop_batch("No metadata for CKM.")
      resp <- e$metadata$variables[[1L]]  # simplified, matches the Python port
      tauargus_set_cell_key_values_cont(
        e$tau, tab, cmd$ckm_p_table_cont, cmd$ckm_p_table_sep,
        resp$ckm_type, resp$ckm_top_k, resp$ckm_zeros, resp$ckm_parity,
        resp$ckm_separation, 0.0, resp$ckm_scaling,
        resp$ckm_sigma0, resp$ckm_sigma1, resp$ckm_xstar, resp$ckm_q,
        resp$ckm_epsilon, cmd$ckm_mu_c
      )
      message("CKM cont: table ", tab + 1L, " processed.")
    }
    e$ckm_tables <- c(e$ckm_tables, tab)
  }

  # =========================================================================
  # Audit (intervalle / realized bounds)
  # =========================================================================
  e$audit <- function(tab) {
    # Compute realized feasibility intervals for table tab (0-based).
    # Port of legacy OptiSuppress.RunAudit.
    b <- e$e_table_cell_bounds(tab)
    tmp <- tempfile("rta_audit_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    jj <- file.path(tmp, "Anneke.JJ")
    if (!isTRUE(tauargus_write_jj_format(e$tau, tab, jj, b[1L], b[2L],
                                         FALSE, FALSE, FALSE)))
      stop_batch("WriteJJFormat failed for AUDIT")
    rows <- audit_jj(jj)

    by_cell <- list()
    for (i in seq_len(nrow(rows))) {
      ok <- tauargus_set_realized_lower_and_upper(e$tau, tab,
                                                  as.integer(rows$cell[i]),
                                                  rows$max[i], rows$min[i])
      if (!isTRUE(ok))
        stop_batch("Audit: could not store realized bounds for cell ",
                   rows$cell[i])
      by_cell[[as.character(rows$cell[i])]] <-
        c(rows$min[i], rows$max[i], as.numeric(isTRUE(rows$unsafe[i])))
    }
    e$audit_map[[as.character(tab)]] <- by_cell
    message("Audit: table ", tab + 1L, " -- ", nrow(rows), " cells audited.")
    rows
  }

  # =========================================================================
  # Recode
  # =========================================================================
  e$recode <- function(cmd) {
    # Apply a recode to a variable (by name, from the current table context).
    if (is.null(e$metadata)) stop_batch("No metadata loaded.")
    vi <- meta_index_of(e$metadata, cmd$var_name)
    if (vi < 0L) stop_batch("Variable '", cmd$var_name, "' not found.")
    var <- e$metadata$variables[[vi + 1L]]

    recode_src <- cmd$recode_file
    # 1) digit truncation level (1..9)
    if (nchar(recode_src) == 1L && recode_src >= "1" && recode_src <= "9") {
      if (var$hierarchical == 0L)
        stop_batch("Truncation is only possible for hierarchical variables")
      e$e_recode_truncate(vi, as.integer(recode_src))
      var$recoded <- TRUE
      e$metadata$variables[[vi + 1L]] <- var
      message("Recode: variable ", var$name, " truncated to level ",
              recode_src, ".")
      return(invisible())
    }

    # 2) file-based recode
    recode_path <- e$e_resolve_path(recode_src)
    first_line <- ""
    if (file.exists(recode_path))
      first_line <- trimws(readLines(recode_path, n = 1L, warn = FALSE,
                                     encoding = "UTF-8"))
    if (first_line == "<TREERECODE>") {
      e$e_recode_tree(vi, recode_path)
      var$recoded <- TRUE
      e$metadata$variables[[vi + 1L]] <- var
      message("Recode: variable ", var$name, " tree-recoded (", recode_src,
              ").")
      return(invisible())
    }

    # 3) classic DoRecode (dest : src spec)
    recode_info <- rta_parse_recode_file(recode_path)
    res <- tauargus_do_recode(e$tau, vi, recode_info$data,
                              recode_info$n_missing, recode_info$missing1,
                              recode_info$missing2)
    if (!isTRUE(res$ok))
      stop_batch("Recode failed for ", var$name, ": ",
                 tauargus_error_string(e$tau, res$error_type),
                 " line ", res$error_line, " pos ", res$error_pos, " ",
                 res$warning)
    var$recoded <- TRUE
    e$metadata$variables[[vi + 1L]] <- var
    tauargus_apply_recode(e$tau)
    message("Recode: variable ", var$name, " recoded (",
            if (nzchar(res$warning)) res$warning else "no warnings", ").")
  }

  e$e_recode_truncate <- function(var_idx, max_level) {
    # Deactivate every node at max_level that has children, then recode.
    tauargus_undo_recode(e$tau, var_idx)
    sz <- tauargus_get_var_number_of_codes(e$tau, var_idx)
    n_codes <- as.integer(sz[1L])
    closed <- 0L
    for (i in seq_len(n_codes)) {
      p <- tauargus_get_var_code_properties(e$tau, var_idx, i - 1L)
      if (isTRUE(p$ok) && p$level == max_level && p$n_children > 0) {
        tauargus_set_var_code_active(e$tau, var_idx, i - 1L, FALSE)
        closed <- closed + 1L
      }
    }
    if (closed > 0L) {
      if (!isTRUE(tauargus_do_active_recode(e$tau, var_idx))) {
        tauargus_undo_recode(e$tau, var_idx)
        stop_batch("There was a problem recoding this variable")
      }
      tauargus_apply_recode(e$tau)
    }
    message("Recode: ", closed, " nodes closed.")
  }

  e$e_recode_tree <- function(var_idx, path) {
    # Deactivate the parent codes listed in a <TREERECODE> file, then recode.
    tauargus_undo_recode(e$tau, var_idx)
    sz <- tauargus_get_var_number_of_codes(e$tau, var_idx)
    n_codes <- as.integer(sz[1L])
    index_by_code <- list()
    for (i in seq_len(n_codes)) {
      p <- tauargus_get_var_code(e$tau, var_idx, i - 1L)
      if (isTRUE(p$ok)) index_by_code[[p$code_string]] <- i
    }
    closed <- 0L
    for (line in readLines(path, warn = FALSE, encoding = "UTF-8")) {
      code <- trimws(line)
      if (!nzchar(code) || code == "<TREERECODE>") next
      ci <- index_by_code[[code]]
      if (is.null(ci)) stop_batch("Code (", code, ") not found")
      tauargus_set_var_code_active(e$tau, var_idx, ci - 1L, FALSE)
      closed <- closed + 1L
    }
    if (closed > 0L) {
      if (!isTRUE(tauargus_do_active_recode(e$tau, var_idx))) {
        tauargus_undo_recode(e$tau, var_idx)
        stop_batch("There was a problem recoding this variable")
      }
      tauargus_apply_recode(e$tau)
    }
    message("Recode: ", closed, " tree nodes closed.")
  }

  # =========================================================================
  # Write
  # =========================================================================
  e$write_intermediate_table <- function(tab, path, simple = FALSE,
                                         holding = FALSE,
                                         suppress_empty = FALSE,
                                         with_audit = FALSE) {
    # Write the legacy WRITETABLE type-5 (INTERMEDIATE) audit file.
    # Row-major walk over active codes; port of TableSet.write.
    if (is.null(e$metadata)) stop_batch("No metadata.")
    if (tab < 0L || tab >= length(e$tables))
      stop_batch("Table ", tab + 1L, " out of range.")
    spec <- e$tables[[tab + 1L]]$spec
    srs <- e$tables[[tab + 1L]]$srs
    meta <- e$metadata
    tau <- e$tau

    exp_vars <- vapply(spec$exp_vars, function(n) meta_index_of(meta, n),
                       integer(1))
    n_exp <- length(exp_vars)

    resp_var <- meta$variables[[meta_index_of(meta, spec$resp_var) + 1L]]
    n_dec <- resp_var$n_decimals
    is_freq <- resp_var$type == "FREQUENCY"
    rounded <- tab %in% e$rounded_tables

    topn <- if (simple) 0L else rta_top_n_needed(srs, holding)

    # Per-dim active-code counts (== native SizeDim after recode).
    max_dim <- integer(n_exp)
    for (j in seq_len(n_exp)) {
      sz <- tauargus_get_var_number_of_codes(tau, exp_vars[j])
      max_dim[j] <- as.integer(sz[2L])
    }

    # Pre-compute code strings for each (var, code_idx).
    code_strings <- vector("list", n_exp)
    for (j in seq_len(n_exp)) {
      vi <- exp_vars[j]
      var <- meta$variables[[vi + 1L]]
      arr <- character(max_dim[j])
      for (ci in seq_len(max_dim[j])) {
        p <- tauargus_get_var_code(tau, vi, ci - 1L)
        if (!isTRUE(p$ok) || !nzchar(p$code_string))
          p$code_string <- if (nzchar(var$tot_code)) var$tot_code else "Total"
        arr[ci] <- paste0("\"", p$code_string, "\"")
      }
      code_strings[[j]] <- arr
    }

    audit_map <- if (with_audit) e$audit_map[[as.character(tab)]] else list()

    conn <- file(path, "w", encoding = "UTF-8")
    on.exit(close(conn), add = TRUE)
    dim_array <- integer(n_exp)
    cell_idx <- 0L
    done <- FALSE
    while (!done) {
      status <- CS_UNKNOWN
      if (n_exp == 0L) {
        # No explanatory variables: single total cell; the native cell read
        # is not used (Python leaves everything at its zero/empty default).
        resp <- 0; rresp <- 0; cta <- 0; ckm <- 0; shadow <- 0; cost <- 0
        key <- 0; key_nz <- 0; freq <- 0
        ms_list <- numeric(0); msw_list <- numeric(0)
        holding_freq <- 0; hms_list <- numeric(0); hnr_list <- integer(0)
        peep_cell <- 0; peep_holding <- 0
        lower <- 0; upper <- 0; rlower <- 0; rupper <- 0
      } else {
        result <- tauargus_get_table_cell(tau, tab, dim_array, topn)
        if (!isTRUE(result$ok)) {
          # Out-of-range dim; skip (should not happen)
          resp <- 0; rresp <- 0; cta <- 0; ckm <- 0; shadow <- 0; cost <- 0
          key <- 0; key_nz <- 0; freq <- 0; status <- CS_EMPTY
          ms_list <- rep(0, topn); msw_list <- rep(0, topn)
          holding_freq <- 0; hms_list <- rep(0, topn)
          hnr_list <- rep(0L, topn)
          peep_cell <- 0; peep_holding <- 0
          lower <- 0; upper <- 0; rlower <- 0; rupper <- 0
        } else {
          resp <- result$resp; rresp <- result$rresp; cta <- result$cta
          ckm <- result$ckm; shadow <- result$shadow; cost <- result$cost
          key <- result$key; key_nz <- result$key_nz
          freq <- result$freq; status <- as.integer(result$status)
          ms_list <- result$ms; msw_list <- result$msw
          holding_freq <- result$holding_freq; hms_list <- result$hms
          hnr_list <- result$hnr
          peep_cell <- result$peep_cell; peep_holding <- result$peep_holding
          lower <- result$lower; upper <- result$upper
          rlower <- result$rlower; rupper <- result$rupper
        }
      }

      skip <- (status == CS_EMPTY && suppress_empty)
      if (!skip) {
        parts <- character(0)
        for (j in seq_len(n_exp))
          parts <- c(parts, code_strings[[j]][dim_array[j] + 1L])
        rv <- if (rounded) rresp else resp
        parts <- c(parts, formatC(rv, format = "f", digits = n_dec))
        if (!is_freq) {
          parts <- c(parts, as.character(if (holding) holding_freq else freq))
          if (!simple)
            parts <- c(parts, formatC(shadow, format = "f", digits = n_dec))
        }
        parts <- c(parts, formatC(cost, format = "f", digits = n_dec))
        if (!simple) {
          if (holding) {
            for (j in seq_len(topn))
              parts <- c(parts, formatC(
                if (j <= length(hms_list)) hms_list[j] else 0.0,
                format = "f", digits = n_dec))
          } else {
            for (j in seq_len(topn))
              parts <- c(parts, formatC(
                if (j <= length(ms_list)) ms_list[j] else 0.0,
                format = "f", digits = n_dec))
          }
        }
        parts <- c(parts, rta_status_symbol(status))
        parts <- c(parts, formatC(lower, format = "f", digits = n_dec))
        parts <- c(parts, formatC(upper, format = "f", digits = n_dec))
        if (with_audit) {
          a <- audit_map[[as.character(cell_idx)]]
          rlo <- if (is.null(a)) 0.0 else a[1L]
          rup <- if (is.null(a)) 0.0 else a[2L]
          parts <- c(parts,
                     formatC(rlo, format = "f", digits = n_dec),
                     formatC(rup, format = "f", digits = n_dec),
                     formatC(rup - rlo, format = "f", digits = n_dec))
          # Legacy: response==0 or safe-not-protected
          # (CellStatus category SAFE_NOT_PROTECTED = {1, 2, 13}) -> zeros;
          # else percentages.
          if (rv == 0 || status %in% c(CS_SAFE, CS_SAFE_MANUAL,
                                       CS_EMPTY_NONSTRUCT))
            parts <- c(parts, "0", "0", "0")
          else
            parts <- c(parts,
                       formatC(100 * (rv - rlo) / rv, format = "f",
                               digits = 2),
                       formatC(100 * (rup - rv) / rv, format = "f",
                               digits = 2),
                       formatC(100 * (rup - rlo) / rv, format = "f",
                               digits = 2))
        }
        writeLines(paste(parts, collapse = ";"), con = conn)
      }

      cell_idx <- cell_idx + 1L
      # advance dim_array (odometer, rightmost = last var)
      k <- n_exp - 1L
      while (k >= 0L) {
        dim_array[k + 1L] <- dim_array[k + 1L] + 1L
        if (dim_array[k + 1L] < max_dim[k + 1L]) break
        dim_array[k + 1L] <- 0L
        k <- k - 1L
      }
      if (k == -1L) done <- TRUE
    }
  }

  e$write_table <- function(cmd) {
    # Write a table to file.
    tab <- as.integer(cmd$tab_no) - 1L
    fpath <- if (nzchar(cmd$file)) {
      # Absolute paths are used as-is (pathlib '/' semantics, like Python).
      if (grepl("^/|[A-Za-z]:[\\\\/]", cmd$file)) cmd$file
      else file.path(e$work_dir, cmd$file)
    } else {
      file.path(e$work_dir, paste0("table_", tab + 1L, ".tab"))
    }

    # Response type follows the legacy SaveTable: 0 = raw values,
    # 1 = rounded (after RND), 3 = cell-key protected. (The legacy
    # batch processor derives it from tableSet.rounded/ckmProtect;
    # writing the rounded response before RND would yield all zeros.)
    resp <- if (tab %in% e$ckm_tables) 3L else
            if (tab %in% e$rounded_tables) 1L else 0L

    if (cmd$output_type == 1L) {
      # CSV
      tauargus_write_csv(e$tau, tab, fpath, TRUE, IDENTITY_DIM_SEQUENCE, resp)
    } else if (cmd$output_type == 5L) {
      # INTERMEDIATE (audit table)
      opts <- cmd$options
      simple <- "+SO" %in% opts
      holding <- "+HI" %in% opts
      suppress_empty <- "+SE" %in% opts
      # AR+: audit the table first (legacy SaveTable runs the intervalle
      # audit right before writing when the flag is set).
      with_audit <- "+AR" %in% opts
      if (with_audit) e$audit(tab)
      e$write_intermediate_table(tab, fpath,
                                 simple = simple,
                                 holding = holding,
                                 suppress_empty = suppress_empty,
                                 with_audit = with_audit)
    } else if (cmd$output_type %in% 2L:7L) {
      # CellRecords (SBS, code value, etc.)
      tauargus_write_cell_records(e$tau, tab, fpath, TRUE, FALSE, FALSE,
                                  "", FALSE, TRUE, resp)
    } else {
      stop_batch("Unknown output type ", cmd$output_type)
    }
    message("Wrote table ", tab + 1L, " to ", fpath)
  }

  # =========================================================================
  # Apriori
  # =========================================================================
  e$e_bogus_range <- function(vi, idx, n_active, levels) {
    # Java CodeList.bogusRange(activeCodeIndex) -- a code plus the run of
    # single-child ancestors below it and single-child descendants above it.
    # levels[i] = c(n_children, level) over all active codes.
    lo <- idx
    while (lo > 0L) {
      pn <- levels[[lo]][1L]; pl <- levels[[lo]][2L]
      cn <- levels[[lo + 1L]][1L]; cl <- levels[[lo + 1L]][2L]
      if (pn == 1 && pl == cl - 1) lo <- lo - 1L
      else break
    }
    hi <- idx
    while (hi + 1L < n_active) {
      pn <- levels[[hi + 1L]][1L]; pl <- levels[[hi + 1L]][2L]
      cn <- levels[[hi + 2L]][1L]; cl <- levels[[hi + 2L]][2L]
      if (pn == 1 && cl == pl + 1) hi <- hi + 1L
      else break
    }
    lo:hi
  }

  e$apply_apriori <- function(cmd) {
    # Apply an apriori file to a computed table.
    # Port of Java APriori.processAprioryFile.
    if (is.null(e$metadata)) stop_batch("No metadata.")
    tab <- as.integer(cmd$tab_no) - 1L
    if (tab < 0L || tab >= length(e$tables))
      stop_batch("Table ", cmd$tab_no, " out of range.")
    spec <- e$tables[[tab + 1L]]$spec
    meta <- e$metadata
    tau <- e$tau
    sep <- cmd$separator

    exp_vars <- vapply(spec$exp_vars, function(n) meta_index_of(meta, n),
                       integer(1))
    n_exp <- length(exp_vars)
    if (n_exp == 0L)
      stop_batch("Table has no exposure variables for apriori.")

    # Per-exp-var: active code strings, (n_children, level) of active codes,
    # and a code-string -> active-index lookup.
    levels <- vector("list", n_exp)
    code_index <- vector("list", n_exp)
    max_dim <- integer(n_exp)
    for (i in seq_len(n_exp)) {
      vi <- exp_vars[i]
      sz <- tauargus_get_var_number_of_codes(tau, vi)
      max_dim[i] <- as.integer(sz[2L])
      var <- meta$variables[[vi + 1L]]
      arr <- character(0)
      lv <- list()
      idx <- list()
      for (ci in seq_len(max_dim[i])) {
        p <- tauargus_get_var_code(tau, vi, ci - 1L)
        if (!isTRUE(p$ok)) next
        cs <- p$code_string
        # Level/n_children only for the bogus range; GetVarCodeProperties
        # can fail on non-hierarchical vars, so fall back to (0, 0).
        pok <- tauargus_get_var_code_properties(tau, vi, ci - 1L)
        lev <- if (isTRUE(pok$ok)) pok$level else 0L
        nch <- if (isTRUE(pok$ok)) pok$n_children else 0L
        arr <- c(arr, cs)
        lv <- c(lv, list(c(nch, lev)))
        # 0-based active code index (arr is 1-based in R).
        pos <- length(arr) - 1L
        # Index on raw, trimmed, and (non-hierarchical) padded forms.
        for (cand in list(cs, trimws(cs), rta_var_normalise_code(var,
                                                                 trimws(cs))))
          if (nzchar(cand) && is.null(idx[[cand]])) idx[[cand]] <- pos
      }
      levels[[i]] <- lv
      code_index[[i]] <- idx
    }

    # Counters mirroring Java aPrioryStatus.
    stats <- list(lines_read = 0L, lines_error = 0L,
                  status_ok = 0L, status_err = 0L,
                  cost_ok = 0L, cost_err = 0L,
                  bounds_ok = 0L, bounds_err = 0L,
                  protlevel_ok = 0L, protlevel_err = 0L)

    apriory_status_map <- list(S = CS_SAFE_MANUAL, U = CS_UNSAFE_MANUAL,
                               P = CS_PROTECT_MANUAL,
                               M = CS_SECONDARY_UNSAFE_MANUAL,
                               ML = CS_SECONDARY_UNSAFE)

    apri_path <- e$e_resolve_arb_path(cmd$file)
    apri_lines <- readLines(apri_path, warn = FALSE, encoding = "UTF-8")

    first_line <- TRUE
    for (line in apri_lines) {
      if (!nzchar(trimws(line))) next
      parts <- unlist(strsplit(line, sep, fixed = TRUE))

      # --- first line: validate separator + field count -------------------
      if (first_line) {
        if (!grepl(sep, line, fixed = TRUE))
          stop_batch("separator (", sep, ") not found in file ", apri_path)
        n_fields <- length(parts) - 1L  # last field is the command
        if (!is.na(suppressWarnings(as.numeric(parts[length(parts)]))))
          n_fields <- n_fields - 1L  # a numeric tail means a value
        if (n_fields != n_exp)
          stop_batch("Apriori file contains ", n_fields,
                     " fields, but the table has ", n_exp, ".")
        first_line <- FALSE
      }

      # --- part 1: read the codes -> dimIndex ------------------------------
      base_dim <- integer(0)
      ok <- TRUE
      for (i in seq_len(n_exp)) {
        var <- meta$variables[[exp_vars[i] + 1L]]
        if (i > length(parts)) {
          ok <- FALSE
          base_dim <- c(base_dim, 0L)
          next
        }
        raw <- trimws(parts[i])
        if (raw == "" || rta_var_is_total_code(var, raw)) {
          base_dim <- c(base_dim, 0L)
          next
        }
        ci <- NULL
        for (cand in list(raw, rta_var_normalise_code(var, raw)))
          if (!is.null(code_index[[i]][[cand]])) {
            ci <- code_index[[i]][[cand]]
            break
          }
        if (is.null(ci)) {
          stats$lines_error <- stats$lines_error + 1L
          if (!cmd$ignore_error)
            stop_batch("Apriori code ", i, " '", raw,
                       "' not found in variable ", var$name, ".")
          ok <- FALSE
          base_dim <- c(base_dim, 0L)
        } else {
          base_dim <- c(base_dim, ci)
        }
      }
      if (ok) stats$lines_read <- stats$lines_read + 1L
      if (!ok) next  # legacy: skip this line

      # --- part 2: the apriory change --------------------------------------
      if (n_exp >= length(parts)) {
        # no change-type token present
        stats$lines_error <- stats$lines_error + 1L
        next
      }
      ap_type <- toupper(trimws(parts[n_exp + 1L]))
      rest <- if (length(parts) >= n_exp + 2L)
        parts[(n_exp + 2L):length(parts)] else character(0)
      x1 <- 0; x2 <- 0
      new_status <- 0

      if (!is.null(apriory_status_map[[ap_type]])) {
        new_status <- apriory_status_map[[ap_type]]
      } else if (ap_type %in% c("C", "W")) {
        new_status <- AP_ADJUST_COST
        if (length(rest) > 0L) x1 <- as.numeric(rest[1L])
      } else if (ap_type == "AB") {
        stop_batch("Apriori bounds (AB) are not implemented (nor in legacy).")
      } else if (ap_type == "PL") {
        new_status <- AP_ADJUST_PROT_LEVEL
        if (length(rest) >= 2L) {
          x1 <- as.numeric(rest[1L])
          x2 <- as.numeric(rest[2L])
        }
      } else {
        stats$lines_error <- stats$lines_error + 1L
        if (!cmd$ignore_error)
          stop_batch("Illegal apriori command '", ap_type,
                     "' in file ", apri_path, ".")
        next
      }

      # --- expand to bogus ranges and apply over the Cartesian product -----
      ranges <- vector("list", n_exp)
      for (i in seq_len(n_exp)) {
        if (cmd$expand_bogus && length(levels[[i]]) > 0L)
          ranges[[i]] <- e$e_bogus_range(exp_vars[i], base_dim[i],
                                        max_dim[i], levels[[i]])
        else
          ranges[[i]] <- base_dim[i]
      }
      prod <- rta_cartesian_product(ranges)
      if (nrow(prod) > 0L)
        for (i in seq_len(nrow(prod)))
          stats <- e$e_apply_apriori_change(tab, prod[i, ], ap_type, x1, x2,
                                  new_status, stats)
    }

    message("Apriori: table ", cmd$tab_no, " -- ", stats$lines_read,
            " lines read, ", stats$status_ok, " status, ", stats$cost_ok,
            " cost, ", stats$protlevel_ok, " prot-level changes applied.")
    stats
  }

  e$e_apply_apriori_change <- function(tab, dim, ap_type, x1, x2, new_status,
                                      stats) {
    # Apply a single apriori change to one cell
    # (APriori.ProcessOneVarCode).
    res <- tauargus_get_table_cell(e$tau, tab, dim, 0L)
    if (!isTRUE(res$ok)) return(stats)
    old_status <- as.integer(res$status)
    if (old_status %in% c(CS_EMPTY, CS_EMPTY_NONSTRUCT))
      return(stats)  # "met status EMPTY mag niks gebeuren"

    if (ap_type %in% c("C", "W")) {  # change cost
      cost <- if (x1 > 0) x1 else 1.0
      if (isTRUE(tauargus_set_table_cell_cost(e$tau, tab, dim, cost)))
        stats$cost_ok <- stats$cost_ok + 1L
      else
        stats$cost_err <- stats$cost_err + 1L
      return(stats)
    }

    if (ap_type == "PL") {
      if (x1 < 0 || x2 < 0) {
        stats$protlevel_err <- stats$protlevel_err + 1L
        return(stats)
      }
      if (!(old_status >= CS_UNSAFE_RULE && old_status <= CS_UNSAFE_MANUAL)) {
        stats$protlevel_err <- stats$protlevel_err + 1L
        return(stats)
      }
      # Apply to the primary-unsafe cell's protection levels (dim-based).
      ok <- isTRUE(tauargus_set_protection_levels_dim(e$tau, tab, dim, x1, x2))
      if (ok) stats$protlevel_ok <- stats$protlevel_ok + 1L
      else stats$protlevel_err <- stats$protlevel_err + 1L
      return(stats)
    }

    if (ap_type %in% c("S", "U", "P", "M", "ML")) {  # status change
      # PROTECTED -> UNSAFE is disallowed by the native setter; also mirror
      # the M/ML -> SECONDARY_UNSAFE_MANUAL mapping from Java.
      if (new_status == CS_SECONDARY_UNSAFE &&
          old_status == CS_SAFE_MANUAL)
        new_status <- CS_SECONDARY_UNSAFE_MANUAL
      if (isTRUE(tauargus_set_table_cell_status_dim(e$tau, tab, dim,
                                                    new_status)))
        stats$status_ok <- stats$status_ok + 1L
      else
        stats$status_err <- stats$status_err + 1L
      return(stats)
    }
  }

  # =========================================================================
  # Batch execution
  # =========================================================================
  e$run_batch <- function(arb_path) {
    # Parse and execute a legacy .arb batch file.
    arb_path <- as.character(arb_path)
    # Resolve file paths relative to the .arb file's directory
    e$work_dir <- dirname(arb_path)
    commands <- parse_batch(arb_path)

    # Reset state
    e$tables <- list()
    e$safety_rules_buf <- list()
    e$rounded_tables <- integer(0)
    e$ckm_tables <- integer(0)
    e$protect_cover_table <- FALSE
    e$audit_map <- list()

    for (cmd in commands) e$e_execute(cmd)
    invisible(e)
  }

  e$e_execute <- function(cmd) {
    kind <- sub("^rta_", "", class(cmd)[1L])
    switch(kind,
      OpenMicrodata = {
        e$data_file <- e$e_resolve_arb_path(cmd$file)
        message("Open microdata: ", cmd$file)
      },
      OpenTableData = {
        e$data_file <- e$e_resolve_arb_path(cmd$file)
        e$is_table <- TRUE
        message("Open table data: ", cmd$file)
      },
      OpenMetadata = {
        meta_path <- e$e_resolve_arb_path(cmd$file)
        e$metadata <- if (e$is_table) rta_parse_rda_table(meta_path)
          else rta_parse_rda(meta_path)
        message("Loaded metadata: ", length(e$metadata$variables),
                " variables.")
        # Set variable indices
        for (i in seq_along(e$metadata$variables))
          e$metadata$variables[[i]]$index <- i - 1L
      },
      SpecifyTable = {
        e$tables <- c(e$tables,
                      list(list(spec = cmd, srs = new_safety_rule_set())))
        e$safety_rules_buf <- c(e$safety_rules_buf, list(list()))
        message("Table ", length(e$tables), ": ",
                paste(cmd$exp_vars, collapse = " "), " | ", cmd$resp_var)
      },
      SafetyRule = {
        if (length(e$safety_rules_buf) > 0L) {
          last <- length(e$safety_rules_buf)
          e$safety_rules_buf[[last]] <-
            c(e$safety_rules_buf[[last]], list(cmd))
        }
      },
      ReadMicrodata = {
        e$e_apply_safety_rules()
        e$open_microdata(e$data_file, e$metadata)
        e$read_microdata()
      },
      ReadTable = {
        e$e_apply_safety_rules()
        e$read_table(cmd$additivity, cmd$keep_status)
      },
      Recode = {
        e$recode(cmd)
      },
      Suppress = {
        e$suppress(cmd)
      },
      WriteTable = {
        e$write_table(cmd)
      },
      GoInteractive = message("Skipping GoInteractive (no-op in headless mode)."),
      Logbook = message("Skipping Logbook (no-op in headless mode)."),
      VersionInfo = message("Skipping VersionInfo (no-op in headless mode)."),
      Anco = message("Skipping Anco (no-op in headless mode)."),
      Clear = {
        e$tables <- list()
        e$safety_rules_buf <- list()
        e$metadata <- NULL
        e$rounded_tables <- integer(0)
        e$ckm_tables <- integer(0)
        e$protect_cover_table <- FALSE
        e$audit_map <- list()
        message("Cleared state.")
      },
      Solver = {
        if (!(cmd$name %in% c("HIGHS", "FREE")))
          message("Solver ", cmd$name,
                  " is not supported; using HiGHS instead (license ignored).")
      },
      Apriory = {
        e$apply_apriori(cmd)
      },
      Cover = {
        # Legacy (batch.java:281): global protect-cover-table flag.
        # CompletedTable(ForCoverTable=true) skips the additivity check and
        # marginals are not recomputed.
        e$protect_cover_table <- TRUE
        message("COVER: protect-cover-table enabled (additivity check skipped).")
      }
    )
  }

  e$e_resolve_arb_path <- function(filename) {
    # Resolve a file path relative to the .arb file's directory.
    p <- filename
    if (file.exists(p)) return(p)
    resolved <- file.path(e$work_dir, p)
    if (file.exists(resolved)) return(resolved)
    filename  # let the caller raise
  }

  e$e_apply_safety_rules <- function() {
    # Aggregate buffered safety rules into each table's SafetyRuleSet.
    if (length(e$safety_rules_buf) > 0L) {
      for (i in seq_along(e$safety_rules_buf)) {
        if (length(e$tables) > 0L && i <= length(e$tables))
          e$tables[[i]]$srs <-
            safety_rule_set_from_rules(e$safety_rules_buf[[i]])
      }
    }
  }

  e
}

# ---------------------------------------------------------------------------
# Standalone helpers used by the engine
# ---------------------------------------------------------------------------

# Split a data line on sep, unquoting each field (Java Tokenizer.nextField).
rta_split_fields <- function(line, sep) {
  out <- character(0)
  rest <- line
  repeat {
    idx <- regexpr(sep, rest, fixed = TRUE)
    if (idx == -1L) {
      value <- trimws(rest)
      rest <- ""
    } else {
      value <- trimws(substring(rest, 1L, idx - 1L))
      rest <- trimws(substring(rest, idx + 1L))
    }
    if (nchar(value) >= 2L && startsWith(value, "\"") &&
        endsWith(value, "\""))
      value <- substring(value, 2L, nchar(value) - 1L)
    out <- c(out, value)
    if (rest == "") break
  }
  out
}

# Java StrUtils.toDouble with comma-decimal support.
rta_to_double <- function(value) {
  v <- gsub(",", ".", trimws(value), fixed = TRUE)
  x <- suppressWarnings(as.numeric(v))
  if (is.na(x)) stop_batch("\"", value, "\" is not numeric.")
  x
}

# Java numberOfTopNNeeded / numberOfHoldingTopNNeeded.
rta_top_n_needed <- function(srs, holding) {
  topn <- 0L
  lo <- if (holding) 2L else 0L
  hi <- if (holding) 4L else 2L
  for (i in lo:(hi - 1L)) {
    if (srs$dom_rule && i < length(srs$dom_n))
      topn <- max(topn, srs$dom_n[i + 1L])
    if (srs$pq_rule && i < length(srs$pq_n) && srs$pq_n[i + 1L] != 0)
      topn <- max(topn, srs$pq_n[i + 1L] + 1L)
  }
  topn
}

# Port of OptiSuppress.correctRoundJJ (pre-rounding fixup of the JJ file).
rta_correct_round_jj <- function(path, base, no_partitions, unit_cost) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  n <- as.integer(trimws(lines[2L]))
  out <- lines[1L:2L]
  for (i in seq_len(n)) {
    ln <- lines[2L + i]
    toks <- unlist(strsplit(trimws(ln), "\\s+"))
    if (length(toks) < 9L) {
      out <- c(out, ln)
      next
    }
    idx <- toks[1L]; resp <- toks[2L]; weight <- toks[3L]; status <- toks[4L]
    lb <- toks[5L]; ub <- toks[6L]; lpl <- toks[7L]; upl <- toks[8L]
    sliding <- toks[9L]
    resp_v <- suppressWarnings(as.numeric(resp))
    if (is.na(resp_v)) resp_v <- 0
    if (unit_cost) weight <- "1"
    if (status == "u" && resp_v < base &&
        suppressWarnings(as.numeric(upl)) < base)
      upl <- formatC(base, format = "f", digits = 6)
    out <- c(out, paste(idx, resp, weight, status, lb, ub, lpl, upl, sliding))
  }
  # Python: rest = lines[2 + n:] (0-based) -> line 3 + n (1-based).
  rest <- if (length(lines) > 2L + n) lines[(3L + n):length(lines)]
  else character(0)
  if (no_partitions && length(rest) > 0L && trimws(rest[1L]) == "1") {
    # the rounder dislikes a lone restriction: duplicate it
    tail <- if (length(rest) >= 3L) rest[3L:length(rest)] else character(0)
    rest <- c("2", rest[2L], rest[2L], tail)
  }
  out <- c(out, rest)
  writeLines(out, path, useBytes = TRUE)
}

# Cartesian product of a list of integer vectors (itertools.product order:
# the last range varies fastest). Returns a n x k integer matrix.
rta_cartesian_product <- function(ranges) {
  if (length(ranges) == 0L) return(matrix(integer(0), nrow = 0L))
  n <- vapply(ranges, length, integer(1))
  if (any(n == 0L)) return(matrix(integer(0), nrow = 0L))
  total <- as.integer(prod(n))
  out <- matrix(0L, nrow = total, ncol = length(ranges))
  pos <- rep(1L, length(ranges))
  for (r in seq_len(total)) {
    for (i in seq_along(ranges)) out[r, i] <- ranges[[i]][pos[i]]
    for (i in length(ranges):1L) {
      pos[i] <- pos[i] + 1L
      if (pos[i] <= n[i]) break
      pos[i] <- 1L
    }
  }
  out
}

# Parse a .grc recode file (port of Variable.readRecodeFile).
rta_parse_recode_file <- function(path) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  tok <- new_tokenizer(text)
  recode_data <- ""
  missing1 <- ""
  missing2 <- ""
  code_list <- ""

  while (!is.null(tok$next_line())) {
    hs <- tok$get_line()
    token <- tok$next_token()
    if (token == "<MISSING>") {
      missing1 <- tok$next_token()
      if (missing1 == "")
        stop_batch("No missing values found after <MISSING>")
      token <- tok$next_token()
      if (token == ",") token <- tok$next_token()
      missing2 <- token
    } else if (token == "<CODELIST>") {
      code_list <- tok$next_token()
    } else if (token != "") {
      recode_data <- paste0(recode_data, hs, "\n")
    }
  }

  n_missing <- 0L
  if (nzchar(missing1)) n_missing <- 1L
  if (nzchar(missing2)) n_missing <- 2L

  list(data = gsub("\n", "\r\n", recode_data, fixed = TRUE),
       missing1 = missing1, missing2 = missing2,
       code_list = code_list, n_missing = n_missing)
}

# ---------------------------------------------------------------------------
# Convenience: one-shot batch run
# ---------------------------------------------------------------------------

run_engine_batch <- function(arb_path) {
  # Parse and execute a legacy .arb batch file with the native engine.
  # Returns the engine (invisible); raises a BatchError on failure.
  eng <- new_engine()
  eng$run_batch(arb_path)
  invisible(eng)
}
