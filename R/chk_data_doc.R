#' @include lists.R

CHECKS$data_doc <- make_check(

  description = "Data objects are documented",
  tags = c("documentation"),
  preps = character(),

  gp = "Document all data objects in your package.",

  check = function(state) {
    path <- state$path
    datadir <- file.path(path, "data")

    if (!dir.exists(datadir)) {
      return(list(status = TRUE, positions = list()))
    }

    datafiles <- list.files(datadir, pattern = "\\.(rda|RData)$")
    if (length(datafiles) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    dataset_names <- tools::file_path_sans_ext(datafiles)

    mandir <- file.path(path, "man")
    rd_files <- if (dir.exists(mandir)) {
      tools::file_path_sans_ext(list.files(mandir, pattern = "\\.Rd$"))
    } else {
      character()
    }

    rox_names <- character()
    rfiles <- r_package_files(path)
    for (f in rfiles) {
      if (!file.exists(f)) next
      lines <- readLines(f, warn = FALSE)
      for (i in seq_along(lines)) {
        if (!grepl("^\\s*#'\\s*@name\\b", lines[i])) next
        nm <- sub("^\\s*#'\\s*@name\\s+", "", lines[i])
        rox_names <- c(rox_names, trimws(nm))
      }
    }

    documented <- union(rd_files, rox_names)
    missing <- setdiff(dataset_names, documented)

    if (length(missing) == 0) {
      list(status = TRUE, positions = list())
    } else {
      problems <- lapply(missing, function(nm) {
        list(
          filename = file.path("data", paste0(nm, ".rda")),
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = nm
        )
      })
      list(status = FALSE, positions = problems)
    }
  }
)
