# ---------------------------------------------------------------------------
# .rda metadata parser + safety-rule aggregation.
#
# Faithful R port of the metadata half of pytauargus/engine.py (Variable,
# Metadata, parse_rda, parse_rda_table, SafetyRuleSet) and its constants.
# ---------------------------------------------------------------------------

# WriteCSV/WriteCSVTable index into DimSequence[d] for each table dimension.
# The native code dereferences it (a null/empty sequence segfaults), so the
# identity sequence {0..9} (Java SaveTable.MAXDIM) is the correct default.
IDENTITY_DIM_SEQUENCE <- 0:9

# ===========================================================================
# Tabular (pre-aggregated) table-input constants
# ===========================================================================
# Cell status values — must match Java CellStatus / native CS_* (defines.h).
CS_UNKNOWN                 <- 0
CS_SAFE                    <- 1
CS_SAFE_MANUAL             <- 2
CS_UNSAFE_RULE             <- 3
CS_UNSAFE_MANUAL           <- 9
CS_PROTECT_MANUAL          <- 10
CS_SECONDARY_UNSAFE        <- 11
CS_SECONDARY_UNSAFE_MANUAL <- 12
CS_EMPTY_NONSTRUCT         <- 13
CS_EMPTY                   <- 14

# Apriori change-type sentinels (Java ``AP_ADJUST_*`` in APriori.java).
AP_ADJUST_COST          <- -1
AP_ADJUST_PROT_LEVEL    <- -2
AP_ADJUST_APRIORI_BOUND <- -3

# Sentinel for an "unparsed" numeric field (Java Cell.UNKNOWN).
CELL_UNKNOWN <- -999999999

# Native error codes returned by SetInCodeList/SetInTable (see defines.h).
ERR_CODENOTINCODELIST <- 1017
ERR_CELLALREADYFILLED <- 1022
ERR_CODEDOESNOTEXIST  <- 1027

# Table additivity modes — must match Java ``TableSet.ADDITIVITY_*``.
# 0 = check additivity, 1 = recompute marginals, 2 = not required (cover).
ADDITIVITY_CHECK        <- 0
ADDITIVITY_RECOMPUTE    <- 1
ADDITIVITY_NOT_REQUIRED <- 2

rta_status_symbol <- function(status) {
  # Intermediate-format status symbol (Java ``CellStatus``/``Category``).
  # ``isEmpty`` (13/14) -> ``E``; else the category symbol: 1,2 -> ``S``;
  # 3-9 -> ``U``; 10 -> ``P``; 11,12 -> ``M``; anything else -> ``?``.
  if (status %in% c(CS_EMPTY_NONSTRUCT, CS_EMPTY)) return("E")
  if (status %in% c(CS_SAFE, CS_SAFE_MANUAL)) return("S")
  if (status >= 3 && status <= 9) return("U")
  if (status == CS_PROTECT_MANUAL) return("P")
  if (status %in% c(CS_SECONDARY_UNSAFE, CS_SECONDARY_UNSAFE_MANUAL)) return("M")
  "?"
}

# ===========================================================================
# Variable (parsed from .rda)
# ===========================================================================
new_variable <- function(name, index = 0L) {
  structure(list(
    name = name, index = index, b_pos = 1L, var_len = 0L,
    n_decimals = 0L, in_table = FALSE, missing = c("", ""),
    tot_code = "",
    # type (mirrors Java Type enum)
    type = "CAT_RESP", # CATEGORICAL, RESPONSE, CAT_RESP, WEIGHT, HOLDING,
                       # REQUEST, SHADOW, COST, FREQUENCY, TOP_N,
                       # LOWER_PROTECTION_LEVEL, UPPER_PROTECTION_LEVEL,
                       # STATUS, RECORD_KEY
    # categorical fields
    has_distance_function = FALSE, distance_function = rep(0L, 5),
    code_list_file = "", hierarchical = 0L, # 0=NONE, 1=LEVELS, 2=FILE
    hier_levels = integer(0), hier_levels_sum = 0L, hier_file_name = "",
    leading_string = ".",
    # CKM / record-key
    p_table_file = "", p_table_file_cont = "", p_table_file_sep = "",
    ckm_type = "N", ckm_top_k = 1L, ckm_scaling = "",
    ckm_sigma0 = -1, ckm_sigma1 = -1, ckm_xstar = -1, ckm_q = -1,
    ckm_epsilon = numeric(0), ckm_separation = FALSE, ckm_zeros = FALSE,
    ckm_parity = FALSE,
    # request
    request_code = c("", ""),
    # misc
    recoded = FALSE, truncatable = FALSE
  ), class = "Variable")
}

rta_var_is_categorical <- function(v) v$type %in% c("CATEGORICAL", "CAT_RESP")
rta_var_is_response    <- function(v) v$type %in% c("RESPONSE", "CAT_RESP")

rta_var_is_numeric <- function(v) {
  # A variable is either categorical or numeric (native SetVariable):
  # CATEGORICAL/CAT_RESP are NOT numeric (the native would then parse
  # their codes with ConvertNumeric and fail with ISNOTNUMERIC).
  v$type %in% c("RESPONSE", "WEIGHT", "SHADOW", "COST", "FREQUENCY",
                "TOP_N", "LOWER_PROTECTION_LEVEL", "UPPER_PROTECTION_LEVEL",
                "RECORD_KEY")
}

rta_var_is_weight     <- function(v) v$type == "WEIGHT"
rta_var_is_holding    <- function(v) v$type == "HOLDING"
rta_var_is_request    <- function(v) v$type == "REQUEST"
rta_var_is_record_key <- function(v) v$type == "RECORD_KEY"

rta_var_n_missings <- function(v) {
  n <- 0L
  while (n < 2L && nzchar(v$missing[n + 1L])) n <- n + 1L
  n
}

rta_var_get_total_code <- function(v)
  if (nzchar(v$tot_code)) v$tot_code else "Total"

rta_var_is_total_code <- function(v, code) {
  # Case-insensitive total-code test (Java ``isTotalCode``).
  tolower(code) == tolower(if (nzchar(v$tot_code)) v$tot_code else "")
}

rta_var_is_missing <- function(v, code) {
  any(vapply(v$missing, function(m) nzchar(m) && code == m, logical(1)))
}

rta_var_normalise_code <- function(v, code) {
  # Pad a non-hierarchical code to ``var_len`` (Java ``normaliseCode``).
  if (nzchar(code) && v$hierarchical == 0L) rta_var_pad_code(v, code) else code
}

rta_var_pad_code <- function(v, code) {
  if (v$var_len > 0L) formatC(code, width = v$var_len, flag = " ") else code
}

# ===========================================================================
# Metadata
# ===========================================================================
new_metadata <- function(is_table = FALSE) {
  structure(list(
    variables = list(), data_file = "", meta_file = "",
    field_separator = ";", is_table = is_table,
    safe_status = "S", unsafe_status = "U", protect_status = "P"
  ), class = "Metadata")
}

meta_find <- function(meta, name) {
  for (v in meta$variables)
    if (tolower(v$name) == tolower(name)) return(v)
  NULL
}

meta_index_of <- function(meta, name) {
  for (i in seq_along(meta$variables))
    if (tolower(meta$variables[[i]]$name) == tolower(name)) return(i - 1L)
  -1L
}

meta_contains <- function(meta, vtype) {
  any(vapply(meta$variables, function(v) v$type == vtype, logical(1)))
}

rta_normalise_missing <- function(missing, var_len) {
  if (trimws(missing) == "") return("")
  if (var_len > 0 && nchar(missing) > var_len)
    stop_batch("Missing value (", missing, ") too long")
  if (var_len > 0) formatC(missing, width = var_len, flag = " ") else missing
}

# ===========================================================================
# .rda parser
# ===========================================================================
rta_parse_rda <- function(path) {
  # Parse a legacy Tau-Argus .rda metadata file.
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  tok <- new_tokenizer(text)
  meta <- new_metadata(is_table = FALSE)
  meta$meta_file <- path
  variable <- NULL
  vi <- 0L
  ckmspecified <- FALSE

  while (!is.null(tok$next_line())) {
    token <- tok$next_token()
    if (token == "") next

    if (startsWith(token, "<")) {
      if (token == "<SEPARATOR>" && tok$line_number == 1L) {
        meta$field_separator <- tok$next_token()
        next
      }
      if (token == "<SPSS>" && tok$line_number == 1L) next

      if (is.null(variable))
        stop_batch("A variable should be declared before giving its properties")

      # -- variable property tags --------------------------------------
      if (token %in% c("<RECODEABLE>", "<RECODABLE>")) {
        if (variable$type == "RESPONSE") variable$type <- "CAT_RESP"
        else variable$type <- "CATEGORICAL"
      } else if (token == "<TOTCODE>") {
        variable$tot_code <- tok$next_token()
      } else if (token == "<DISTANCE>") {
        variable$has_distance_function <- TRUE
        for (i in 1:5) {
          t <- tok$next_token()
          if (t == "")
            variable$distance_function[i] <-
              if (i == 1L) 1L else variable$distance_function[i - 1L]
          else
            variable$distance_function[i] <- as.integer(t)
        }
      } else if (token == "<CODELIST>") {
        variable$code_list_file <- tok$next_token()
      } else if (token == "<HIERARCHICAL>") {
        variable$hierarchical <- 2L # HIER_FILE
      } else if (token == "<HIERLEVELS>") {
        variable$hierarchical <- 1L # HIER_LEVELS
        x <- 0L
        repeat {
          t <- tok$next_token()
          if (t == "") break
          n <- as.integer(t)
          variable$hier_levels <- c(variable$hier_levels, n)
          x <- x + n
        }
        variable$hier_levels_sum <- x
      } else if (token == "<HIERLEADSTRING>") {
        variable$leading_string <- tok$next_token()
      } else if (token == "<HIERCODELIST>") {
        variable$hier_file_name <- tok$next_token()
      } else if (token == "<NUMERIC>") {
        hs <- tok$next_token()
        if (hs == "") {
          if (variable$type == "CATEGORICAL") variable$type <- "CAT_RESP"
          else variable$type <- "RESPONSE"
        } else if (hs == "<SHADOW>") {
          variable$type <- "SHADOW"
        } else if (hs %in% c("<COST>", "<COSTVAR>")) {
          variable$type <- "COST"
        } else if (hs == "<LOWERPL>") {
          variable$type <- "LOWER_PROTECTION_LEVEL"
        } else if (hs == "<UPPERPL>") {
          variable$type <- "UPPER_PROTECTION_LEVEL"
        }
      } else if (token == "<WEIGHT>") {
        variable$type <- "WEIGHT"
      } else if (token == "<HOLDING>") {
        variable$type <- "HOLDING"
      } else if (token == "<REQUEST>") {
        variable$type <- "REQUEST"
        variable$request_code <- c(tok$next_token(), tok$next_token())
      } else if (token == "<DECIMALS>") {
        variable$n_decimals <- as.integer(tok$next_token())
      } else if (token == "<TRUNCABLE>") {
        variable$truncatable <- TRUE
      } else if (token == "<RECORDKEY>") {
        variable$type <- "RECORD_KEY"
      } else if (token %in% c("<PFILE_FREQ>", "<PFILE>")) {
        variable$p_table_file <- tok$next_token()
      } else if (token == "<PFILE_CONT>") {
        variable$p_table_file_cont <- tok$next_token()
      } else if (token == "<PFILE_SEP>") {
        variable$p_table_file_sep <- tok$next_token()
      } else if (token == "<CKM>") {
        if (!rta_var_is_response(variable))
          stop_batch("<CKM> tag only allowed for numeric variables.")
        hs <- toupper(tok$next_field("("))
        ckmspecified <- TRUE
        if (hs == "N") {
          ckmspecified <- FALSE
        } else if (hs %in% c("M", "D", "V")) {
          variable$ckm_type <- hs
        } else if (hs == "T") {
          variable$ckm_type <- hs
          variable$ckm_top_k <- as.integer(tok$next_field(")"))
        } else {
          stop_batch("Unknown type <CKM> ", hs,
                     " for variable ", variable$name)
        }
        if (ckmspecified)
          variable$ckm_epsilon <- rep(1.0, variable$ckm_top_k)
      } else if (token == "<INCLUDEZEROS>") {
        if (!ckmspecified)
          stop_batch("<INCLUDEZEROS> only allowed after <CKM>.")
        variable$ckm_zeros <- tok$next_token() == "Y"
      } else if (token == "<PARITY>") {
        if (!ckmspecified)
          stop_batch("<PARITY> only allowed after <CKM>.")
        variable$ckm_parity <- tok$next_token() == "Y"
      } else if (token == "<SEPARATION>") {
        if (!ckmspecified)
          stop_batch("<SEPARATION> only allowed after <CKM>.")
        variable$ckm_separation <- tok$next_token() == "Y"
      } else if (token == "<SCALING>") {
        if (!ckmspecified)
          stop_batch("<SCALING> only allowed after <CKM>.")
        hs <- toupper(tok$next_field("("))
        variable$ckm_scaling <- hs
        if (hs == "F") {
          variable$ckm_sigma0 <- as.numeric(tok$next_field(","))
          variable$ckm_sigma1 <- as.numeric(tok$next_field(","))
          variable$ckm_xstar <- as.numeric(tok$next_field(","))
          t <- variable$ckm_top_k
          if (t >= 2L) {
            variable$ckm_q <- as.numeric(tok$next_field(","))
            if (t >= 3L)
              for (i in 2L:(t - 1L))
                variable$ckm_epsilon[i] <- as.numeric(tok$next_field(","))
            variable$ckm_epsilon[t] <- as.numeric(tok$next_field(")"))
          } else {
            variable$ckm_q <- as.numeric(tok$next_field(")"))
          }
        } else if (hs == "N") {
          t <- variable$ckm_top_k
          if (t >= 2L) {
            variable$ckm_sigma1 <- as.numeric(tok$next_field(","))
            if (t >= 3L)
              for (i in 2L:(t - 1L))
                variable$ckm_epsilon[i] <- as.numeric(tok$next_field(","))
            variable$ckm_epsilon[t] <- as.numeric(tok$next_field(")"))
          } else {
            variable$ckm_sigma1 <- as.numeric(tok$next_field(")"))
          }
        } else {
          stop_batch("Unknown type <SCALING> ", hs,
                     " for variable ", variable$name)
        }
      } else {
        stop_batch("Unknown keyword (", token, ") in line ", tok$line_number)
      }
      meta$variables[[vi]] <- variable
    } else {
      # -- new variable declaration ------------------------------------
      variable <- new_variable(name = token, index = length(meta$variables))
      meta$variables <- c(meta$variables, list(variable))
      vi <- length(meta$variables)
      ckmspecified <- FALSE
      variable$b_pos <- as.integer(tok$next_token())
      variable$var_len <- as.integer(tok$next_token())
      variable$missing <- c(
        rta_normalise_missing(tok$next_token(), variable$var_len),
        rta_normalise_missing(tok$next_token(), variable$var_len)
      )
      meta$variables[[vi]] <- variable
    }
  }

  tok$close()
  meta
}

# ===========================================================================
# Tabular (pre-aggregated) .rda parser
# ===========================================================================
rta_parse_rda_table <- function(path) {
  # Parse a tabular .rda metadata file. Mirrors
  # ``Metadata.readTableMetadata``. Variable declarations carry no
  # ``bPos``/``varLen`` (only name + two missing values); ``<NUMERIC>`` may be
  # followed by a subtype (``<SHADOW>``/``<COSTVAR>``/``<LOWERPL>``/
  # ``<UPPERPL>``), and top-level ``<SAFE>``/``<UNSAFE>``/``<PROTECT>`` tags
  # set the status characters.
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  tok <- new_tokenizer(text)
  meta <- new_metadata(is_table = TRUE)
  meta$meta_file <- path
  meta$field_separator <- ";"
  variable <- NULL
  vi <- 0L

  while (!is.null(tok$next_line())) {
    token <- tok$next_token()
    if (token == "") next

    if (startsWith(token, "<")) {
      if (token == "<SEPARATOR>") {
        meta$field_separator <- rta_unquote(tok$next_token())
        next
      }
      if (token == "<SAFE>") {
        meta$safe_status <- rta_unquote(tok$next_token())
        next
      }
      if (token == "<UNSAFE>") {
        meta$unsafe_status <- rta_unquote(tok$next_token())
        next
      }
      if (token == "<PROTECT>") {
        meta$protect_status <- rta_unquote(tok$next_token())
        next
      }

      if (is.null(variable))
        stop_batch("A variable should be declared before giving its properties")

      if (token %in% c("<RECODEABLE>", "<RECODABLE>")) {
        variable$type <- "CATEGORICAL"
        variable$in_table <- TRUE
      } else if (token == "<TOTCODE>") {
        variable$tot_code <- rta_unquote(tok$next_token())
      } else if (token == "<DISTANCE>") {
        variable$has_distance_function <- TRUE
        for (i in 1:5) {
          t <- tok$next_token()
          if (t == "")
            variable$distance_function[i] <-
              if (i == 1L) 1L else variable$distance_function[i - 1L]
          else
            variable$distance_function[i] <- as.integer(t)
        }
      } else if (token == "<CODELIST>") {
        variable$code_list_file <- rta_unquote(tok$next_token())
      } else if (token == "<HIERARCHICAL>") {
        variable$hierarchical <- 2L
      } else if (token == "<HIERLEVELS>") {
        variable$hierarchical <- 1L
        x <- 0L
        repeat {
          t <- tok$next_token()
          if (t == "") break
          n <- as.integer(t)
          variable$hier_levels <- c(variable$hier_levels, n)
          x <- x + n
        }
        variable$hier_levels_sum <- x
      } else if (token == "<HIERLEADSTRING>") {
        variable$leading_string <- rta_unquote(tok$next_token())
      } else if (token == "<HIERCODELIST>") {
        variable$hier_file_name <- rta_unquote(tok$next_token())
      } else if (token == "<NUMERIC>") {
        sub1 <- tok$next_token()
        if (sub1 == "") {
          variable$type <- "RESPONSE"
        } else if (sub1 == "<SHADOW>") {
          variable$type <- "SHADOW"
        } else if (sub1 %in% c("<COST>", "<COSTVAR>")) {
          variable$type <- "COST"
        } else if (sub1 == "<LOWERPL>") {
          variable$type <- "LOWER_PROTECTION_LEVEL"
        } else if (sub1 == "<UPPERPL>") {
          variable$type <- "UPPER_PROTECTION_LEVEL"
        }
      } else if (token == "<FREQUENCY>") {
        variable$type <- "FREQUENCY"
      } else if (token == "<MAXSCORE>") {
        variable$type <- "TOP_N"
      } else if (token == "<STATUS>") {
        variable$type <- "STATUS"
      } else if (token == "<DECIMALS>") {
        variable$n_decimals <- as.integer(tok$next_token())
      } else {
        stop_batch("Unknown keyword (", token, ") in line ", tok$line_number)
      }
      meta$variables[[vi]] <- variable
    } else {
      # -- new variable declaration (no bPos/varLen) --------------------
      variable <- new_variable(name = token, index = length(meta$variables))
      meta$variables <- c(meta$variables, list(variable))
      vi <- length(meta$variables)
      variable$missing <- c(
        rta_normalise_missing(tok$next_token(), 0L),
        rta_normalise_missing(tok$next_token(), 0L)
      )
      meta$variables[[vi]] <- variable
    }
  }

  tok$close()
  meta
}

# ===========================================================================
# Safety rule aggregation (per-table)
# ===========================================================================
new_safety_rule_set <- function() {
  structure(list(
    dom_rule = FALSE, dom_n = integer(0), dom_k = integer(0),
    pq_rule = FALSE, pq_p = integer(0), pq_q = integer(0),
    pq_n = integer(0),
    min_freq = integer(0), freq_marge = integer(0),
    peep_rule = FALSE, peep_percentage = integer(0),
    peep_marge = integer(0), peep_min_freq = integer(0),
    apply_peep = FALSE, apply_weight = FALSE, weight_on_safety = FALSE,
    apply_holding = FALSE, zero_rule = FALSE, zero_range = 0,
    empty_as_ns = FALSE, ns_range = 10L,
    # Legacy TableSet.manualMarge = 20 (manual p.123: "The default value = 20"):
    # protection margin for manually unsafe cells when no MAN rule is given.
    manual_perc = 20L
  ), class = "SafetyRuleSet")
}

safety_rule_set_from_rules <- function(rules) {
  # Build a SafetyRuleSet from the parsed batch SafetyRule objects.
  s <- new_safety_rule_set()
  n_pq <- 0L
  n_dom <- 0L
  n_freq <- 0L
  n_req <- 0L

  for (rule in rules) {
    args <- as.integer(rule$args)
    if (rule$kind == "P") {
      if (n_pq >= 4L) stop_batch("More than 4 P rules specified.")
      if (length(args) == 2L) {
        s$pq_p <- c(s$pq_p, args[1L])
        s$pq_q <- c(s$pq_q, 100L)
        s$pq_n <- c(s$pq_n, args[2L])
      } else if (length(args) == 3L) {
        s$pq_p <- c(s$pq_p, args[1L])
        s$pq_q <- c(s$pq_q, args[2L])
        s$pq_n <- c(s$pq_n, args[3L])
      } else {
        stop_batch("P rule needs 2 or 3 parameters.")
      }
      s$pq_rule <- TRUE
      if (n_pq >= 2L && s$pq_p[n_pq + 1L] > 0) s$apply_holding <- TRUE
      n_pq <- n_pq + 1L
    } else if (rule$kind == "NK") {
      if (n_dom >= 4L) stop_batch("More than 4 NK rules specified.")
      if (length(args) != 2L) stop_batch("NK rule needs 2 parameters.")
      s$dom_n <- c(s$dom_n, args[1L])
      s$dom_k <- c(s$dom_k, args[2L])
      s$dom_rule <- TRUE
      if (n_dom >= 2L) s$apply_holding <- TRUE
      n_dom <- n_dom + 1L
    } else if (rule$kind == "FREQ") {
      if (n_freq >= 2L) stop_batch("More than 2 frequency rules specified.")
      if (length(args) != 2L) stop_batch("FREQ rule needs 2 parameters.")
      s$min_freq <- c(s$min_freq, args[1L])
      s$freq_marge <- c(s$freq_marge, args[2L])
      if (n_freq >= 1L && s$min_freq[n_freq + 1L] > 0)
        s$apply_holding <- TRUE
      n_freq <- n_freq + 1L
    } else if (rule$kind == "ZERO") {
      if (length(args) != 1L) stop_batch("ZERO rule needs 1 parameter.")
      s$zero_range <- as.numeric(args[1L])
      s$zero_rule <- TRUE
    } else if (rule$kind == "WGT") {
      if (length(args) != 1L) stop_batch("WGT needs 1 parameter.")
      k <- args[1L]
      if (k < 0L || k > 3L) stop_batch("WGT value should be 0-3.")
      s$apply_weight <- k != 0L
      s$weight_on_safety <- k >= 2L
    } else if (rule$kind == "MIS") {
      # missingIsSafe — handled at set_table level, not safety
      NULL
    } else if (rule$kind == "MAN") {
      if (length(args) != 1L) stop_batch("MAN needs 1 parameter.")
      s$manual_perc <- args[1L]
    } else if (rule$kind == "REQ") {
      if (n_req >= 2L) stop_batch("More than 2 request rules specified.")
      if (length(args) != 4L) stop_batch("REQ rule needs 4 parameters.")
      s$peep_percentage <- c(s$peep_percentage, args[1L], args[2L])
      s$peep_marge <- c(s$peep_marge, args[4L])
      s$peep_min_freq <- c(s$peep_min_freq, args[3L])
      if (args[1L] > 0 || args[3L] > 0) s$apply_peep <- TRUE
      if (n_req >= 0L && args[1L] > 0) s$apply_holding <- TRUE
      n_req <- n_req + 1L
    } else {
      stop_batch("Unknown safety rule kind: ", rule$kind)
    }
  }

  s
}
