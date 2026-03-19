#' @include lists.R

extract_param_docs <- function(path) {
  rdir <- file.path(path, "R")
  if (!dir.exists(rdir)) return(data.frame(
    param = character(), desc = character(),
    file = character(), line = integer(),
    stringsAsFactors = FALSE
  ))

  rfiles <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE)
  params <- list()

  for (f in rfiles) {
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    roxygen_idx <- grep("^#'\\s*@param\\s+", lines)

    for (idx in roxygen_idx) {
      m <- regmatches(lines[idx],
        regexec("^#'\\s*@param\\s+(\\S+)\\s+(.*)", lines[idx]))[[1]]
      if (length(m) < 3) next

      param_name <- m[2]
      desc_lines <- m[3]

      j <- idx + 1L
      while (j <= length(lines) && grepl("^#'\\s+[^@]", lines[j])) {
        continuation <- sub("^#'\\s+", " ", lines[j])
        desc_lines <- paste0(desc_lines, continuation)
        j <- j + 1L
      }

      params[[length(params) + 1]] <- data.frame(
        param = param_name,
        desc = trimws(desc_lines),
        file = f,
        line = idx,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(params) == 0) return(data.frame(
    param = character(), desc = character(),
    file = character(), line = integer(),
    stringsAsFactors = FALSE
  ))
  do.call(rbind, params)
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
    dupes <- pd$key[duplicated(pd$key)]
    if (length(dupes) == 0) return(list(status = TRUE, positions = list()))

    dup_rows <- pd[pd$key %in% dupes, ]
    seen <- character()
    problems <- list()

    for (i in seq_len(nrow(dup_rows))) {
      key <- dup_rows$key[i]
      if (key %in% seen) next
      seen <- c(seen, key)

      matches <- dup_rows[dup_rows$key == key, ]
      files <- unique(basename(matches$file))
      if (length(files) < 2) next

      for (j in seq_len(nrow(matches))) {
        problems[[length(problems) + 1]] <- list(
          filename = file.path("R", basename(matches$file[j])),
          line_number = matches$line[j],
          column_number = NA_integer_,
          ranges = list(),
          line = paste0("@param ", matches$param[j])
        )
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)
