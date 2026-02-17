#' @include lists.R
#' @importFrom roxygen2 parse_package

uses_roxygen2 <- function(path) {
  desc_path <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_path)) return(FALSE)
  fields <- names(read.dcf(desc_path)[1, ])
  any(grepl("^Roxygen", fields))
}

find_function_defs <- function(path) {
  rfiles <- r_package_files(path)
  fn_re <- "^([A-Za-z0-9_.]+)\\s*(?:<-|=)\\s*function\\b"
  defs <- list()

  for (f in rfiles) {
    if (!file.exists(f)) next
    lines <- readLines(f, warn = FALSE)
    for (i in seq_along(lines)) {
      m <- regmatches(lines[i], regexec(fn_re, lines[i]))[[1]]
      if (length(m) >= 2) {
        defs[[length(defs) + 1]] <- data.frame(
          name = m[2],
          file = f,
          line = i,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(defs) == 0) {
    return(data.frame(
      name = character(), file = character(),
      line = integer(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, defs)
}

parse_roxygen2 <- function(path) {
  if (!uses_roxygen2(path)) {
    stop("Package does not use roxygen2.")
  }

  parse_messages <- character()
  blocks <- withCallingHandlers(
    roxygen2::parse_package(path, env = NULL),
    message = function(m) {
      msg <- conditionMessage(m)
      if (grepl("is not a known tag", msg)) {
        parse_messages <<- c(parse_messages, msg)
        invokeRestart("muffleMessage")
      }
    }
  )

  ns <- tryCatch(
    parseNamespaceFile(basename(path), dirname(path)),
    error = function(e) {
      list(
        exports = character(),
        S3methods = matrix(character(), ncol = 3, nrow = 0)
      )
    }
  )

  s3m <- ns$S3methods
  s3methods <- if (nrow(s3m) > 0) {
    paste0(s3m[, 1], ".", s3m[, 2])
  } else {
    character()
  }

  list(
    blocks = blocks,
    namespace_exports = ns$exports,
    namespace_s3methods = s3methods,
    function_defs = find_function_defs(path),
    parse_messages = parse_messages
  )
}

PREPS$roxygen2 <- function(state, path = state$path, quiet) {
  state$roxygen2 <- try(parse_roxygen2(path), silent = quiet)

  if (inherits(state$roxygen2, "try-error")) {
    warning("Prep step for roxygen2 failed.")
  }
  state
}
