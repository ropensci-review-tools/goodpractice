#' @include lists.R

CHECKS$export_or_nord <- make_check(

  description = "Functions have roxygen @export or @noRd tags",
  tags = c("documentation"),
  preps = character(),

  gp = "Tag every function with either @export or @noRd.",

  check = function(state) {
    path <- state$path

    nsfile <- file.path(path, "NAMESPACE")
    exports <- character()
    if (file.exists(nsfile)) {
      lines <- readLines(nsfile, warn = FALSE)
      ex <- regmatches(lines, regexec("^\\s*export\\(([^)]+)\\)", lines))
      exports <- vapply(
        ex, function(x) if (length(x) >= 2) x[2] else "", ""
      )
      exports <- trimws(exports)
      exports <- exports[nzchar(exports)]
    }

    problems <- list()

    rfiles <- r_package_files(path)
    for (f in rfiles) {
      if (!file.exists(f)) next
      lines <- readLines(f, warn = FALSE)

      for (i in seq_along(lines)) {
        ln <- lines[i]
        m <- regexec("^([A-Za-z0-9_.]+)\\s*(?:<-|=)\\s*function\\b", ln)
        mo <- regmatches(ln, m)[[1]]
        if (length(mo) == 0) next
        name <- mo[2]

        j <- i - 1
        while (j > 0 && grepl("^\\s*$", lines[j])) j <- j - 1
        rox <- character()
        while (j > 0 && grepl("^\\s*#'", lines[j])) {
          rox <- c(lines[j], rox)
          j <- j - 1
        }

        has_export <- any(grepl("@export\\b", rox))
        has_nord <- any(grepl("@noRd\\b", rox))
        has_rdname <- any(grepl("@rdname\\b", rox))
        in_namespace <- name %in% exports

        if (!has_export && !has_nord && !has_rdname && !in_namespace) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(f)),
            line_number = i,
            column_number = NA_integer_,
            ranges = list(),
            line = ln
          )
        }
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)

CHECKS$nord_has_keywords_internal <- make_check(

  description = "Functions tagged @noRd also have @keywords internal",
  tags = c("documentation"),
  preps = character(),

  gp = "Add @keywords internal alongside @noRd for internal functions.",

  check = function(state) {
    path <- state$path
    problems <- list()

    rfiles <- r_package_files(path)
    for (f in rfiles) {
      if (!file.exists(f)) next
      lines <- readLines(f, warn = FALSE)

      for (i in seq_along(lines)) {
        ln <- lines[i]
        m <- regexec("^([A-Za-z0-9_.]+)\\s*(?:<-|=)\\s*function\\b", ln)
        mo <- regmatches(ln, m)[[1]]
        if (length(mo) == 0) next

        j <- i - 1
        while (j > 0 && grepl("^\\s*$", lines[j])) j <- j - 1
        rox <- character()
        while (j > 0 && grepl("^\\s*#'", lines[j])) {
          rox <- c(lines[j], rox)
          j <- j - 1
        }

        has_nord <- any(grepl("@noRd\\b", rox))
        has_keywords_internal <- any(grepl("@keywords\\s+internal\\b", rox))

        if (has_nord && !has_keywords_internal) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(f)),
            line_number = i,
            column_number = NA_integer_,
            ranges = list(),
            line = ln
          )
        }
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)

CHECKS$export_and_keywords_internal <- make_check(

  description = "@export and @keywords internal should not co-exist",
  tags = c("documentation"),
  preps = character(),

  gp = paste(
    "Remove @keywords internal from exported functions, or remove @export",
    "if the function is meant to be internal."
  ),

  check = function(state) {
    path <- state$path
    problems <- list()

    rfiles <- r_package_files(path)
    for (f in rfiles) {
      if (!file.exists(f)) next
      lines <- readLines(f, warn = FALSE)

      for (i in seq_along(lines)) {
        ln <- lines[i]
        m <- regexec("^([A-Za-z0-9_.]+)\\s*(?:<-|=)\\s*function\\b", ln)
        mo <- regmatches(ln, m)[[1]]
        if (length(mo) == 0) next

        j <- i - 1
        while (j > 0 && grepl("^\\s*$", lines[j])) j <- j - 1
        rox <- character()
        while (j > 0 && grepl("^\\s*#'", lines[j])) {
          rox <- c(lines[j], rox)
          j <- j - 1
        }

        has_export <- any(grepl("@export\\b", rox))
        has_keywords_internal <- any(grepl("@keywords\\s+internal\\b", rox))

        if (has_export && has_keywords_internal) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(f)),
            line_number = i,
            column_number = NA_integer_,
            ranges = list(),
            line = ln
          )
        }
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)
