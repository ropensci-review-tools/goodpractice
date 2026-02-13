## Check: exported functions (non-methods) should have a @return roxygen tag

CHECKS$docs_return <- make_check(

  description = "Exported functions should have a @return roxygen tag",
  tags = c("documentation"),
  preps = c("expressions", "namespace"),

  gp = paste(
    "Document return values for exported (non-method)",
    "functions using @return."
  ),

  check = function(state) {
    path <- state$path

    # gather exports from NAMESPACE if present
    nsfile <- file.path(path, "NAMESPACE")
    exports <- character()
    s3methods <- character()
    if (file.exists(nsfile)) {
      lines <- readLines(nsfile, warn = FALSE)
      # export(name)
      ex <- regmatches(lines, regexec("^\\s*export\\(([^)]+)\\)", lines))
      ex <- vapply(ex, function(x) if (length(x) >= 2) x[2] else "", "")
      ex <- trimws(ex)
      ex <- ex[nzchar(ex)]
      if (length(ex)) exports <- c(exports, ex)

      # S3method(generic, class)
      s3_re <- "^\\s*S3method\\(([^,]+),\\s*([^)]+)\\)"
      sm <- regmatches(lines, regexec(s3_re, lines))
      sm <- vapply(sm, function(x) {
        if (length(x) >= 3) {
          paste0(trimws(x[2]), ".", trimws(x[3]))
        } else {
          ""
        }
      }, "")
      sm <- sm[nzchar(sm)]
      if (length(sm)) s3methods <- c(s3methods, sm)
    }

    problems <- list()

    rfiles <- r_package_files(path)
    for (f in rfiles) {
      if (!file.exists(f)) next
      lines <- readLines(f, warn = FALSE)

      # find top-level function definitions
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

        has_return <- any(grepl("@return\\b", rox))
        has_export_tag <- any(grepl("@export\\b", rox))
        has_method_tag <- any(grepl("@method\\b|@S3method\\b", rox))
        has_rdname <- any(grepl("@rdname\\b", rox))
        has_describein <- any(grepl("@describeIn\\b", rox))

        exported <- (name %in% exports) || has_export_tag
        is_s3method <- (name %in% s3methods) ||
          has_method_tag || grepl("\\.", name)

        skip <- is_s3method || has_rdname || has_describein
        if (exported && !skip && !has_return) {
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
