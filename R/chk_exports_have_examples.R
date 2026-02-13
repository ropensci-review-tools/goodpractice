#' @include lists.R

CHECKS$exports_have_examples <- make_check(

  description = "Exported functions have @examples or @example",
  tags = c("documentation"),
  preps = character(),

  gp = "Add examples to all exported functions using @examples or @example.",

  check = function(state) {
    path <- state$path

    nsfile <- file.path(path, "NAMESPACE")
    exports <- character()
    s3methods <- character()
    if (file.exists(nsfile)) {
      lines <- readLines(nsfile, warn = FALSE)
      ex <- regmatches(lines, regexec("^\\s*export\\(([^)]+)\\)", lines))
      exports <- vapply(
        ex, function(x) if (length(x) >= 2) x[2] else "", ""
      )
      exports <- trimws(exports)
      exports <- exports[nzchar(exports)]

      sm <- regmatches(
        lines, regexec("^\\s*S3method\\(([^,]+),\\s*([^)]+)\\)", lines)
      )
      s3methods <- vapply(
        sm,
        function(x) {
          if (length(x) >= 3) {
            paste0(trimws(x[2]), ".", trimws(x[3]))
          } else {
            ""
          }
        },
        ""
      )
      s3methods <- s3methods[nzchar(s3methods)]
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
        has_examples <- any(grepl("@examples?\\b", rox))
        has_rdname <- any(grepl("@rdname\\b", rox))
        has_describein <- any(grepl("@describeIn\\b", rox))
        in_namespace <- name %in% exports
        is_s3method <- name %in% s3methods

        exported <- has_export || in_namespace

        skip <- is_s3method || has_rdname || has_describein
        if (exported && !skip && !has_examples) {
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
