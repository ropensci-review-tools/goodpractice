#' @include lists.R
#' @importFrom utils getParseData

vignette_files <- function(path) {
  vigdir <- file.path(path, "vignettes")
  if (!dir.exists(vigdir)) return(character())
  list.files(vigdir, pattern = "\\.(Rmd|Rnw|qmd)$",
             full.names = TRUE, recursive = TRUE)
}

is_skipped_chunk <- function(lines, start, end) {
  header <- lines[start]
  if (grepl("eval\\s*=\\s*(FALSE|F)\\b", header) ||
      grepl("purl\\s*=\\s*(FALSE|F)\\b", header)) {
    return(TRUE)
  }
  if (end <= start + 1L) return(FALSE)
  body <- lines[seq(start + 1L, end - 1L)]
  hashpipe <- body[grepl("^#\\|", body)]
  any(grepl("eval\\s*:\\s*(false|FALSE|F)\\b", hashpipe)) ||
    any(grepl("purl\\s*:\\s*(false|FALSE|F)\\b", hashpipe))
}

match_chunk_pairs <- function(starts, ends) {
  if (length(starts) == 0) return(matrix(integer(0), ncol = 2))
  if (length(starts) != length(ends)) {
    message("Chunk start/end indices failed sanity checks in vignette")
    return(matrix(integer(0), ncol = 2))
  }

  chunks <- cbind(start = starts, end = ends)

  ok <- all(chunks[, 2] > chunks[, 1])
  if (nrow(chunks) > 1) {
    ok <- ok &&
      all(diff(chunks[, 1]) > 0) &&
      all(diff(chunks[, 2]) > 0) &&
      all(chunks[-1, 1] > chunks[-nrow(chunks), 2])
  }

  if (!ok) {
    message("Chunk start/end indices failed sanity checks in vignette")
    return(matrix(integer(0), ncol = 2))
  }

  chunks
}

extract_vignette_code <- function(f) {
  lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
  if (is.null(lines)) return(NULL)
  n <- length(lines)
  ext <- tolower(tools::file_ext(f))

  if (ext %in% c("rmd", "qmd")) {
    all_starts <- grep("^```\\s*\\{\\s*r\\b", lines)
    fence_ends <- grep("^```\\s*$", lines)
  } else if (ext == "rnw") {
    all_starts <- grep("^<<.*>>=\\s*$", lines)
    fence_ends <- grep("^@\\s*$", lines)
  } else {
    return(NULL)
  }

  all_chunks <- match_chunk_pairs(all_starts, fence_ends)
  if (nrow(all_chunks) == 0) return(NULL)

  keep <- !vapply(seq_len(nrow(all_chunks)), function(i) {
    is_skipped_chunk(lines, all_chunks[i, 1], all_chunks[i, 2])
  }, logical(1))
  chunks <- all_chunks[keep, , drop = FALSE]
  if (nrow(chunks) == 0) return(NULL)

  output <- rep("", n)
  indices <- unlist(apply(chunks, 1, function(i) {
    if (i[2] - i[1] <= 1) return(integer(0))
    seq(i[1] + 1L, i[2] - 1L)
  }))
  output[indices] <- lines[indices]

  if (all(output == "")) return(NULL)
  output
}

vignette_parse_data <- function(f) {
  code_lines <- extract_vignette_code(f)
  if (is.null(code_lines)) return(NULL)

  parsed <- safe_parse(text = code_lines, keep_source = TRUE)
  if (is.null(parsed) || length(parsed) == 0) return(NULL)

  getParseData(parsed)
}

call_descendants <- function(pd, fn_call_id) {
  name_expr <- pd$parent[pd$id == fn_call_id]
  call_expr <- pd$parent[pd$id == name_expr]

  ids <- call_expr
  queue <- call_expr
  while (length(queue) > 0) {
    children <- pd$id[pd$parent %in% queue]
    if (length(children) == 0) break
    ids <- c(ids, children)
    queue <- children
  }
  ids
}

check_vignette_calls <- function(state, fn_name, nested_fn = NULL) {
  problems <- list()

  for (f in names(state$vignette)) {
    entry <- state$vignette[[f]]
    pd <- entry$parse_data
    orig_lines <- entry$lines

    fn_rows <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == fn_name, ,
                  drop = FALSE]
    if (nrow(fn_rows) == 0) next

    for (i in seq_len(nrow(fn_rows))) {
      if (!is.null(nested_fn)) {
        desc_ids <- call_descendants(pd, fn_rows$id[i])
        desc <- pd[pd$id %in% desc_ids, , drop = FALSE]
        if (!any(desc$token == "SYMBOL_FUNCTION_CALL" &
                 desc$text == nested_fn)) {
          next
        }
      }

      ln <- fn_rows$line1[i]
      line_text <- if (ln <= length(orig_lines)) orig_lines[ln] else ""

      problems[[length(problems) + 1]] <- list(
        filename = file.path("vignettes", basename(f)),
        line_number = ln,
        column_number = fn_rows$col1[i],
        ranges = list(),
        line = trimws(line_text)
      )
    }
  }

  if (length(problems) == 0) {
    list(status = TRUE, positions = list())
  } else {
    list(status = FALSE, positions = problems)
  }
}

CHECKS$vignette_no_rm_list <- make_check(

  description = "Vignettes do not use rm(list = ls())",
  tags = c("best practice", "warning"),
  preps = "vignette",

  gp = paste(
    "do not use rm(list = ls()) in vignettes.",
    "Vignettes run in their own environment;",
    "clearing the workspace is unnecessary and confusing for users."
  ),

  check = function(state) {
    check_vignette_calls(state, "rm", nested_fn = "ls")
  }
)

CHECKS$vignette_no_setwd <- make_check(

  description = "Vignettes do not use setwd()",
  tags = c("best practice", "warning"),
  preps = "vignette",

  gp = paste(
    "do not use setwd() in vignettes.",
    "Changing the working directory makes vignettes fragile",
    "and non-reproducible on other machines."
  ),

  check = function(state) {
    check_vignette_calls(state, "setwd")
  }
)
