#' @include lists.R treesitter.R

extract_param_docs <- function(path) {
  rdir <- file.path(path, "R")
  if (!dir.exists(rdir)) return(data.frame(
    param = character(), desc = character(),
    file = character(), line = integer(),
    stringsAsFactors = FALSE
  ))

  rfiles <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE)

  params <- unlist(lapply(rfiles, function(f) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    roxygen_idx <- grep("^#'\\s*@param\\s+", lines)

    lapply(roxygen_idx, function(idx) {
      m <- regmatches(lines[idx],
        regexec("^#'\\s*@param\\s+(\\S+)\\s+(.*)", lines[idx]))[[1]]
      if (length(m) < 3) return(NULL)

      desc_lines <- m[3]
      j <- idx + 1L
      while (j <= length(lines) && grepl("^#'\\s+[^@]", lines[j])) {
        desc_lines <- paste0(desc_lines, sub("^#'\\s+", " ", lines[j]))
        j <- j + 1L
      }

      data.frame(
        param = m[2], desc = trimws(desc_lines),
        file = f, line = idx, stringsAsFactors = FALSE
      )
    })
  }), recursive = FALSE)

  params <- Filter(Negate(is.null), params)
  if (length(params) == 0) return(data.frame(
    param = character(), desc = character(),
    file = character(), line = integer(),
    stringsAsFactors = FALSE
  ))
  do.call(rbind, params)
}

cross_file_duplicates <- function(df, key_col, file_col) {
  duped_keys <- unique(df[[key_col]][duplicated(df[[key_col]])])
  if (length(duped_keys) == 0) return(df[0, , drop = FALSE])

  candidates <- df[df[[key_col]] %in% duped_keys, , drop = FALSE]
  multi_file <- vapply(duped_keys, function(k) {
    length(unique(basename(candidates[[file_col]][candidates[[key_col]] == k]))) >= 2
  }, logical(1))

  candidates[candidates[[key_col]] %in% duped_keys[multi_file], , drop = FALSE]
}

CHECKS$roxygen2_duplicate_params <- make_check(

  description = "Avoid duplicated @param documentation across functions",
  tags = c("documentation", "roxygen2"),
  preps = character(),

  gp = paste(
    "use @inheritParams to avoid duplicating parameter documentation.",
    "Identical @param descriptions across files suggest shared docs",
    "should be inherited from a single source."
  ),

  check = function(state) {
    pd <- extract_param_docs(state$path)
    if (nrow(pd) == 0) return(list(status = TRUE, positions = list()))

    pd$key <- paste(pd$param, pd$desc, sep = "|||")
    dupes <- cross_file_duplicates(pd, "key", "file")
    if (nrow(dupes) == 0) return(list(status = TRUE, positions = list()))

    problems <- lapply(seq_len(nrow(dupes)), function(i) {
      list(
        filename = file.path("R", basename(dupes$file[i])),
        line_number = dupes$line[i],
        column_number = NA_integer_,
        ranges = list(),
        line = paste0("@param ", dupes$param[i])
      )
    })

    list(status = FALSE, positions = problems)
  }
)

## --------------------------------------------------------------------

normalize_body_text <- function(fn_node) {
  body <- treesitter::node_child_by_field_name(fn_node, "body")
  if (is.null(body)) return("")
  gsub("\\s+", " ", trimws(treesitter::node_text(body)))
}

CHECKS$duplicate_function_bodies <- make_check(

  description = "No functions with identical bodies",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "consolidate functions with identical bodies into a single",
    "shared helper to reduce code duplication."
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$functions) < 2) {
      return(list(status = TRUE, positions = list()))
    }

    bodies <- vapply(ts$functions, function(fn) {
      normalize_body_text(fn$fn_node)
    }, character(1))

    trivial <- nchar(bodies) < 20
    bodies[trivial] <- paste0("__trivial__", seq_along(bodies)[trivial])

    fn_df <- data.frame(
      body = bodies,
      name = vapply(ts$functions, `[[`, "", "name"),
      file = vapply(ts$functions, `[[`, "", "file"),
      line = vapply(ts$functions, function(fn) fn$line, numeric(1)),
      stringsAsFactors = FALSE
    )

    dupes <- cross_file_duplicates(fn_df, "body", "file")
    if (nrow(dupes) == 0) return(list(status = TRUE, positions = list()))

    problems <- lapply(seq_len(nrow(dupes)), function(i) {
      list(
        filename = file.path("R", basename(dupes$file[i])),
        line_number = dupes$line[i],
        column_number = NA_integer_,
        ranges = list(),
        line = dupes$name[i]
      )
    })

    list(status = FALSE, positions = problems)
  }
)
