## Check: exported functions (non-methods) should have a @return roxygen tag

CHECKS$docs_return <- make_check(

  description = "Exported functions should have a @return roxygen tag",
  tags = c("documentation"),
  preps = c("expressions", "namespace"),

  gp = "Document return values for exported (non-method) functions using @return.",

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
      sm <- regmatches(lines, regexec("^\\s*S3method\\(([^,]+),\\s*([^)]+)\\)", lines))
      sm <- vapply(sm, function(x) if (length(x) >= 3) paste0(trimws(x[2]), ".", trimws(x[3])) else "", "")
      sm <- sm[nzchar(sm)]
      if (length(sm)) s3methods <- c(s3methods, sm)
    }

    problems <- list()

    rfiles <- r_package_files(path)
    for (f in rfiles) {
      if (!file.exists(f)) next
      lines <- readLines(f, warn = FALSE)

      # find function definitions of the form 'name <- function' or 'name = function'
      for (i in seq_along(lines)) {
        ln <- lines[i]
        m <- regexec("^([A-Za-z0-9_.]+)\\s*(?:<-|=)\\s*function\\b", ln)
        mo <- regmatches(ln, m)[[1]]
        if (length(mo) == 0) next
        name <- mo[2]

        # collect roxygen block immediately above
        j <- i - 1
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
        is_s3method <- (paste0(name) %in% s3methods) || has_method_tag || grepl("\\.", name)

        if (exported && !is_s3method && !has_rdname && !has_describein && !has_return) {
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
