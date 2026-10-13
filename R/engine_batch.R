# ---------------------------------------------------------------------------
# Legacy-compatible Tau-Argus .arb batch file parser.
#
# Faithful R port of the grammar in pytauargus/batch.py (itself a port of the
# legacy Java front-end: batch.java + Tokenizer.java). Same keywords, argument
# forms and status-machine ordering rules: a .arb file written for the legacy
# front-end parses identically here.
#
# This file is a *parser only*. It tokenises and validates a batch file and
# emits a list of command objects (named lists with a class attribute).
# Execution of those commands against the native engine lives in
# engine_run.R.
# ---------------------------------------------------------------------------

# Status machine (mirrors batch.java's `status` int)
#   0 = start
#   1 = data / table file found
#   2 = meta file found
#   3 = specify-tables found
#   4 = safety rule found
STATUS_START   <- 0
STATUS_DATA    <- 1
STATUS_META    <- 2
STATUS_SPECIFY <- 3
STATUS_SAFETY  <- 4

# NB: no extra stop() arguments -- stop(cond, call. = FALSE) warns
# "additional arguments ignored in stop()"; the call is hidden via the
# condition's own call = NULL field.
stop_batch <- function(...) {
  stop(structure(
    list(message = paste0(...), call = NULL),
    class = c("BatchError", "error", "condition")))
}

# ===========================================================================
# Tokenizer — 1:1 port of argus.utils.Tokenizer
# ===========================================================================
new_tokenizer <- function(text) {
  lines <- unlist(strsplit(text, "\r\n|\r|\n", perl = TRUE))
  tok <- new.env(parent = emptyenv())
  tok$lines <- lines
  tok$pos <- 0L
  tok$line <- ""
  tok$line_number <- 0L

  # Read the next non-empty, non-comment line (or NULL at EOF).
  # Blank lines and lines starting with "//" are skipped; tabs are
  # normalised to spaces and the line is stripped.
  tok$next_line <- function() {
    n <- length(tok$lines)
    while (tok$pos < n) {
      tok$pos <- tok$pos + 1L
      tok$line_number <- tok$line_number + 1L
      raw <- trimws(gsub("\t", " ", tok$lines[tok$pos], fixed = TRUE))
      if (raw == "" || startsWith(raw, "//")) next
      tok$line <- raw
      return(raw)
    }
    tok$line <- ""
    NULL
  }

  tok$close <- function() {
    tok$lines <- character(0)
  }

  # Read the next token from the current line.
  tok$next_token <- function() {
    line <- tok$line
    if (startsWith(line, "//")) {
      tok$line <- ""
      return("")
    }
    if (startsWith(line, "\"")) {
      rest <- substring(line, 2L)
      q <- regexpr("\"", rest, fixed = TRUE)
      if (q == -1L) {
        value <- rest
        tok$line <- ""
      } else {
        value <- substring(rest, 1L, q - 1L)
        tok$line <- trimws(substring(rest, q + 1L))
      }
    } else if (nzchar(line) &&
               substring(line, 1L, 1L) %in% c(",", "|", "(", ")")) {
      value <- substring(line, 1L, 1L)
      tok$line <- trimws(substring(line, 2L))
    } else {
      sp <- regexpr(" ", line, fixed = TRUE)
      if (sp == -1L) {
        value <- line
        tok$line <- ""
      } else {
        value <- substring(line, 1L, sp - 1L)
        tok$line <- trimws(substring(line, sp + 1L))
      }
    }
    if (startsWith(value, "<")) value <- toupper(value)
    value
  }

  # Read the next field delimited by `separator`.
  tok$next_field <- function(separator) {
    idx <- regexpr(separator, tok$line, fixed = TRUE)
    if (idx == -1L) {
      value <- trimws(tok$line)
      tok$line <- ""
    } else {
      value <- trimws(substring(tok$line, 1L, idx - 1L))
      tok$line <- trimws(substring(tok$line, idx + 1L))
    }
    if (nchar(value) >= 2L && startsWith(value, "\"") &&
        endsWith(value, "\"")) {
      value <- substring(value, 2L, nchar(value) - 1L)
    }
    value
  }

  # Read the next single character.
  tok$next_char <- function() {
    if (nchar(tok$line) == 1L) {
      value <- tok$line
      tok$line <- ""
    } else {
      value <- substring(tok$line, 1L, 1L)
      tok$line <- trimws(substring(tok$line, 2L))
    }
    value
  }

  tok$test_next_char <- function() {
    if (nzchar(tok$line)) substring(tok$line, 1L, 1L) else ""
  }

  tok$get_line <- function() tok$line

  tok$clear_line <- function() {
    tok$line <- ""
    invisible(NULL)
  }
  tok
}

# ===========================================================================
# tail-string helpers (mirror batch.java's `String[] tail` + nextToken/nextChar)
# ===========================================================================
TAIL_SEPARATORS <- c("(", ",", ")", "|")

new_tail <- function(s) {
  t <- new.env(parent = emptyenv())
  t$s <- s
  t
}

# Return the token before the first ( , ) | separator in tail$s.
tail_next_token <- function(t) {
  s <- t$s
  if (s == "") return("")
  pmin <- 100000L # 0-based position of the separator; sentinel if none
  for (ch in TAIL_SEPARATORS) {
    p <- regexpr(ch, s, fixed = TRUE)
    if (p > 0L && (p - 1L) < pmin) pmin <- p - 1L
  }
  if (pmin == 100000L) {
    t$s <- ""
    stop_batch("No separator found in string ")
  }
  hs <- trimws(substring(s, 1L, pmin))
  t$s <- trimws(substring(s, pmin + 1L))
  if (nchar(hs) >= 2L && startsWith(hs, "\"") && endsWith(hs, "\""))
    hs <- substring(hs, 2L, nchar(hs) - 1L)
  hs
}

tail_next_char <- function(t) {
  s <- t$s
  if (s == "") return("")
  t$s <- substring(s, 2L)
  substring(s, 1L, 1L)
}

rta_unquote <- function(s) {
  s <- trimws(s)
  if (nchar(s) >= 2L && startsWith(s, "\"") && endsWith(s, "\""))
    s <- substring(s, 2L, nchar(s) - 1L)
  s
}

# ===========================================================================
# Command constructors (port of the *Command dataclasses)
# ===========================================================================
# NB: the first formal is named `tag` (not `kind`) so that a command field
# called `kind` (e.g. Suppress) lands in `...` instead of binding to the
# class-determining formal argument.
rta_cmd <- function(tag, ...) {
  structure(list(...), class = c(paste0("rta_", tag), "rta_command"))
}

new_open_microdata  <- function(file) rta_cmd("OpenMicrodata", file = file)
new_open_table_data <- function(file) rta_cmd("OpenTableData", file = file)
new_open_metadata   <- function(file) rta_cmd("OpenMetadata", file = file)
new_read_microdata  <- function() rta_cmd("ReadMicrodata")
new_cover           <- function() rta_cmd("Cover")
new_go_interactive  <- function() rta_cmd("GoInteractive")
new_anco            <- function() rta_cmd("Anco")
new_clear           <- function() rta_cmd("Clear")

new_specify_table <- function(exp_vars, resp_var, shadow_var = "",
                              cost_var = "", lam = 0) {
  rta_cmd("SpecifyTable", exp_vars = exp_vars, resp_var = resp_var,
          shadow_var = shadow_var, cost_var = cost_var, lam = lam)
}

new_safety_rule <- function(kind, args)
  rta_cmd("SafetyRule", kind = kind, args = args)

new_read_table <- function(additivity = 0, keep_status = FALSE)
  rta_cmd("ReadTable", additivity = additivity, keep_status = keep_status)

new_apriory <- function(file, tab_no, separator, ignore_error = TRUE,
                        expand_bogus = FALSE) {
  rta_cmd("Apriory", file = file, tab_no = tab_no, separator = separator,
          ignore_error = ignore_error, expand_bogus = expand_bogus)
}

new_suppress <- function(kind, tab_no) {
  rta_cmd(
    "Suppress", kind = kind, tab_no = tab_no, params = character(0),
    # normalised, filled by the specific _parse_* helpers:
    gh_apriory_pct = 0, gh_size = 0, gh_singleton = FALSE,
    mod_max_time = 0, mod_singleton = FALSE, mod_singleton_multi = FALSE,
    mod_min_freq = FALSE, mod_msc = 0, mod_lower_marg = 0,
    mod_upper_marg = 0,
    opt_max_time = 0,
    rnd_base = 0, rnd_step = 0, rnd_time = 10, rnd_partitions = 0,
    rnd_stop_rule = 2, rnd_unit_cost = TRUE,
    ckm_p_table = "", ckm_p_table_cont = "", ckm_p_table_sep = "",
    ckm_mu_c = 0
  )
}

new_write_table <- function(tab_no, output_type, options = character(0),
                            file = "") {
  rta_cmd("WriteTable", tab_no = tab_no, output_type = output_type,
          options = options, file = file)
}

new_recode_cmd <- function(tab_no, var_name, recode_file) {
  rta_cmd("Recode", tab_no = tab_no, var_name = var_name,
          recode_file = recode_file)
}

new_solver <- function(name, license_file = "")
  rta_cmd("Solver", name = name, license_file = license_file)

new_logbook    <- function(path) rta_cmd("Logbook", path = path)
new_version_info <- function(path) rta_cmd("VersionInfo", path = path)

# ===========================================================================
# Command argument parsers (mirror batch.java's per-case logic)
# ===========================================================================
rta_parse_specify_table <- function(tok) {
  exp_vars <- character(0)
  repeat {
    name <- tok$next_token() # handles a leading quoted name internally
    exp_vars <- c(exp_vars, name)
    c <- tok$test_next_char()
    if (!(c %in% c("|", "\"")))
      stop_batch("A '|' or a '\"' is expected here (got '", c, "')")
    if (c == "|") break
    # c == '"' -> next iteration reads the following quoted var name
  }

  tok$next_char() # consume '|'
  resp <- tok$next_field("|")

  shadow <- ""
  cost <- ""
  lam <- 0
  if (tok$get_line() != "") {
    shadow <- tok$next_field("|")
    if (tok$get_line() != "") cost <- tok$next_field(",")
    rest <- tok$get_line()
    if (rest != "") lam <- as.numeric(rest)
  }

  new_specify_table(exp_vars = exp_vars, resp_var = resp,
                    shadow_var = shadow, cost_var = cost, lam = lam)
}

rta_parse_safety_rule <- function(tok) {
  rules <- list()
  rule_type <- toupper(tok$next_field("("))
  if (rule_type == "") return(rules) # no rule -> use given status

  # Re-assemble the remaining "(....|....)" remainder.
  t <- new_tail(paste0("(", tok$get_line()))
  tok$clear_line()

  while (rule_type != "") {
    c <- tail_next_char(t)
    if (c != "(") stop_batch("A ( is expected here")
    token <- tail_next_token(t)
    args <- character(0)
    args <- c(args, token)
    # consume comma-separated args until the closing ')'
    repeat {
      c <- tail_next_char(t)
      if (c == ",") {
        args <- c(args, tail_next_token(t))
        next
      }
      if (c == ")") break
      if (c == "") break
      stop_batch("A ',' or ')' is expected here (got '", c, "')")
    }
    rules <- c(rules, list(new_safety_rule(rule_type, args)))

    c <- tail_next_char(t) # expect '|' or ''
    if (!(c %in% c("|", "")))
      stop_batch("A '|' is expected here (got '", c, "')")
    if (c == "") break
    rule_type <- toupper(tail_next_token(t))
  }

  rules
}

rta_parse_suppress <- function(tok) {
  kind <- toupper(tok$next_field("("))
  t <- new_tail(tok$get_line())
  tok$clear_line()

  token <- tail_next_token(t)
  tab_no <- as.integer(token)
  cmd <- new_suppress(kind, tab_no)

  if (kind == "GH") {
    c <- tail_next_char(t)
    if (c == ",") {
      cmd$gh_apriory_pct <- as.integer(tail_next_token(t))
      c <- tail_next_char(t)
    }
    if (c == ",") {
      cmd$gh_size <- as.integer(tail_next_token(t))
      c <- tail_next_char(t)
    }
    if (c == ",") {
      cmd$gh_singleton <- tail_next_token(t) == "1"
      c <- tail_next_char(t)
    }
  } else if (kind == "MOD") {
    c <- tail_next_char(t)
    if (c == ",") {
      cmd$mod_max_time <- as.integer(tail_next_token(t))
      c <- tail_next_char(t)
    }
    i <- 0L
    while (c == "," && i < 3L) {
      i <- i + 1L
      val <- tail_next_token(t)
      if (i == 1L) cmd$mod_singleton <- val == "1"
      else if (i == 2L) cmd$mod_singleton_multi <- val == "1"
      else if (i == 3L) cmd$mod_min_freq <- val == "1"
      c <- tail_next_char(t)
    }
    if (c == ",") { # MSC, LOWERMARG, UPPERMARG
      msc <- tail_next_token(t)
      if (msc != "") cmd$mod_msc <- as.numeric(msc)
      c <- tail_next_char(t)
      if (c == ",") {
        lm <- tail_next_token(t)
        if (lm != "") cmd$mod_lower_marg <- as.numeric(lm)
        c <- tail_next_char(t)
      }
      if (c == ",") {
        um <- tail_next_token(t)
        if (um != "") cmd$mod_upper_marg <- as.numeric(um)
      }
    }
  } else if (kind == "OPT") {
    c <- tail_next_char(t)
    if (c == ",") cmd$opt_max_time <- as.integer(tail_next_token(t))
  } else if (kind == "RND") {
    c <- tail_next_char(t)
    if (c != ",") stop_batch("a comma(,) expected after RND base")
    cmd$rnd_base <- as.integer(tail_next_token(t))
    c <- tail_next_char(t)
    if (c == ",") { # steps
      cmd$rnd_step <- as.integer(tail_next_token(t))
      c <- tail_next_char(t)
    }
    if (c == ",") { # max time
      cmd$rnd_time <- as.integer(tail_next_token(t))
      c <- tail_next_char(t)
    }
    if (c == ",") { # partitions
      cmd$rnd_partitions <- as.integer(tail_next_token(t))
      c <- tail_next_char(t)
    }
    if (c == ",") { # stop rule
      cmd$rnd_stop_rule <- as.integer(tail_next_token(t))
      c <- tail_next_char(t)
    }
    if (c == ",") { # unit cost
      cmd$rnd_unit_cost <- tail_next_token(t) == "1"
    }
  } else if (kind == "CKM") {
    c <- tail_next_char(t)
    if (c != ")") stop_batch("A ) is expected here for CKM")
    rem <- trimws(t$s)
    if (rem == "") return(cmd)
    # magnitude tables use | separators; freq tables a single filename
    if (grepl("|", rem, fixed = TRUE)) {
      parts <- unlist(strsplit(rem, "|", fixed = TRUE))
      p1 <- trimws(parts[1L])
      if (nzchar(p1) && !startsWith(p1, "//"))
        cmd$ckm_p_table_cont <- rta_unquote(p1)
      if (length(parts) > 1L && nzchar(trimws(parts[2L])) &&
          !startsWith(trimws(parts[2L]), "//")) {
        cmd$ckm_p_table_sep <- rta_unquote(trimws(parts[2L]))
      }
      if (length(parts) > 2L && nzchar(trimws(parts[3L])))
        cmd$ckm_mu_c <- as.numeric(trimws(parts[3L]))
    } else {
      hs <- rta_unquote(rem)
      if (!startsWith(hs, "//")) cmd$ckm_p_table <- hs
    }
  }

  rem <- trimws(t$s)
  cmd$params <- if (nzchar(rem)) rem else character(0)
  cmd
}

rta_parse_write_table <- function(tok) {
  t <- new_tail(tok$get_line())
  tok$clear_line()
  c <- tail_next_char(t)
  if (c != "(") stop_batch("( expected; not a ", c)
  tab_no <- as.integer(tail_next_token(t))
  c <- tail_next_char(t)
  if (c != ",") stop_batch(", expected; not a ", c)
  output_type <- as.integer(tail_next_token(t))
  if (output_type < 1L || output_type > 7L)
    stop_batch("Unknown output type (", output_type, ")")
  c <- tail_next_char(t)
  if (c != ",") stop_batch(", expected; not a ", c)

  options <- character(0)
  repeat {
    hs <- toupper(tail_next_token(t))
    if (hs == "") break
    if (nchar(hs) < 3L) stop_batch("Unknown option (", hs, ")")
    opt <- substring(hs, 1L, 2L)
    sign <- substring(hs, 3L, 3L)
    if (!(sign %in% c("+", "-")))
      stop_batch("+ or - expected; not a ", sign)
    options <- c(options, paste0(sign, opt))
  }

  c <- tail_next_char(t)
  if (c != ",") stop_batch(", expected; not a ", c)
  file_name <- tail_next_token(t)
  new_write_table(tab_no = tab_no, output_type = output_type,
                  options = options, file = file_name)
}

rta_parse_recode <- function(tok) {
  t <- new_tail(tok$get_line())
  tok$clear_line()
  token <- tail_next_token(t)
  tab_no <- as.integer(token)
  c <- tail_next_char(t)
  if (c != ",") stop_batch("Illegal character ", c, " found. Comma expected")
  var_name <- tail_next_token(t)
  c <- tail_next_char(t)
  if (c != ",") stop_batch("Illegal character ", c, " found. comma expected")
  recode_file <- rta_unquote(t$s)
  new_recode_cmd(tab_no = tab_no, var_name = var_name,
                 recode_file = recode_file)
}

rta_parse_apriory <- function(tok) {
  f_name <- tok$next_token()
  c <- tok$next_char()
  if (c != ",") stop_batch("a , was expected here")
  hs <- tok$next_field(",")
  tab_no <- as.integer(hs)
  if (tok$test_next_char() != "\"")
    stop_batch("A quote was expected here before the separator")
  tok$next_char() # consume opening quote
  # The separator is a quoted string (legacy form: "," / ";");
  # read it up to the closing quote.
  idx <- regexpr("\"", tok$line, fixed = TRUE)
  if (idx == -1L) {
    sep <- trimws(tok$line)
    tok$line <- ""
  } else {
    sep <- trimws(substring(tok$line, 1L, idx - 1L))
    tok$line <- trimws(substring(tok$line, idx + 1L))
  }
  if (sep == "") stop_batch("No separator was specified")
  ignore_error <- TRUE
  expand_bogus <- FALSE
  c <- tok$next_char()
  if (c != "") {
    if (c != ",") stop_batch("A comma was expected here")
    hs <- tok$next_field(",")
    if (hs != "") {
      if (hs == "1") ignore_error <- TRUE
      else if (hs == "0") ignore_error <- FALSE
      else stop_batch("Illegal field (", hs, ") for ignore error")
    }
    hs <- tok$next_field(",")
    if (hs != "") expand_bogus <- (hs == "1")
  }
  new_apriory(file = f_name, tab_no = tab_no, separator = sep,
              ignore_error = ignore_error, expand_bogus = expand_bogus)
}

rta_parse_read_table <- function(tok) {
  t <- new_tail(tok$next_token())
  c <- tail_next_char(t)
  if (c == "") c <- "0"
  if (!(c %in% c("0", "1", "2")))
    stop_batch("Illegal parameter (", c, ") for ReadTable")
  additivity <- as.integer(c)
  keep_status <- FALSE
  if (t$s != "" && additivity == 1L) {
    c <- tail_next_char(t)
    if (c %in% c("T", "t")) keep_status <- TRUE
    else if (!(c %in% c("F", "f"))) stop_batch("Invalid option ", c)
  }
  new_read_table(additivity = additivity, keep_status = keep_status)
}

# ===========================================================================
# Top-level parser
# ===========================================================================
parse_batch <- function(path) {
  # Parse a legacy .arb batch file into a list of command objects.
  # Raises a BatchError on malformed syntax or an out-of-order command
  # (the same positions the legacy front-end rejected).
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  tok <- new_tokenizer(text)
  commands <- list()
  status <- STATUS_START
  data_file <- ""

  while (!is.null(tok$next_line())) {
    token <- tok$next_token()
    if (startsWith(token, "\\\\") || startsWith(token, "//"))
      next # comment

    if (token == "<OPENMICRODATA>") {
      if (status != STATUS_START)
        stop_batch(token, " is not allowed in this position")
      data_file <- tok$next_token()
      status <- STATUS_DATA
      commands <- c(commands, list(new_open_microdata(data_file)))
    } else if (token == "<OPENTABLEDATA>") {
      if (!(status %in% c(STATUS_START, STATUS_SAFETY)))
        stop_batch(token, " is not allowed in this position")
      data_file <- tok$next_token()
      status <- STATUS_DATA
      commands <- c(commands, list(new_open_table_data(data_file)))
    } else if (token == "<OPENMETADATA>") {
      if (data_file == "")
        stop_batch("A data file must be specified first")
      if (status != STATUS_DATA)
        stop_batch(token, " is not allowed in this position")
      meta <- tok$next_token()
      status <- STATUS_META
      data_file <- ""
      commands <- c(commands, list(new_open_metadata(meta)))
    } else if (token == "<SPECIFYTABLE>") {
      if (!(status %in% c(STATUS_META, STATUS_SAFETY)))
        stop_batch(token, " is not allowed in this position")
      commands <- c(commands, list(rta_parse_specify_table(tok)))
      status <- STATUS_SPECIFY
    } else if (token == "<SAFETYRULE>") {
      if (status != STATUS_SPECIFY)
        stop_batch(token, " is not allowed in this position")
      rules <- rta_parse_safety_rule(tok)
      status <- STATUS_SAFETY
      commands <- c(commands, rules)
    } else if (token == "<READMICRODATA>") {
      if (status != STATUS_SAFETY)
        stop_batch(token, " is not allowed in this position")
      commands <- c(commands, list(new_read_microdata()))
    } else if (token == "<READTABLE>") {
      if (status == STATUS_SPECIFY) status <- STATUS_SAFETY
      if (status != STATUS_SAFETY)
        stop_batch(token, " is not allowed in this position")
      commands <- c(commands, list(rta_parse_read_table(tok)))
    } else if (token %in% c("<APRIORY>", "<APRIORI>")) {
      commands <- c(commands, list(rta_parse_apriory(tok)))
    } else if (token == "<COVER>") {
      commands <- c(commands, list(new_cover()))
    } else if (token == "<SUPPRESS>") {
      if (status != STATUS_SAFETY)
        stop_batch(token, " is not allowed in this position")
      commands <- c(commands, list(rta_parse_suppress(tok)))
    } else if (token == "<WRITETABLE>") {
      commands <- c(commands, list(rta_parse_write_table(tok)))
    } else if (token == "<RECODE>") {
      commands <- c(commands, list(rta_parse_recode(tok)))
    } else if (token == "<SOLVER>") {
      name <- toupper(tok$next_field(","))
      lic <- tok$next_field(",")
      commands <- c(commands,
                    list(new_solver(name = name,
                                    license_file = rta_unquote(lic))))
    } else if (token == "<GOINTERACTIVE>") {
      commands <- c(commands, list(new_go_interactive()))
    } else if (token == "<LOGBOOK>") {
      commands <- c(commands, list(new_logbook(rta_unquote(tok$get_line()))))
      tok$clear_line()
    } else if (token == "<VERSIONINFO>") {
      commands <- c(commands, list(new_version_info(rta_unquote(tok$get_line()))))
      tok$clear_line()
    } else if (token == "<ANCO>") {
      commands <- c(commands, list(new_anco()))
    } else if (token == "<JJ>") {
      next # not implemented in legacy either; skipped
    } else if (token == "<CLEAR>") {
      commands <- c(commands, list(new_clear()))
      status <- STATUS_START
    } else if (token == "<COMMENT>") {
      next
    } else {
      stop_batch("Unknown keyword ", token)
    }
  }

  tok$close()
  commands
}
