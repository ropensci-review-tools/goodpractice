
#' @include lists.R

CHECKS$has_readme <- make_check(

  description = "README file exists",
  tags = c("info", "documentation"),
  preps = "package_structure",

  gp = 'add a {.file README.md} (or {.file README.Rmd}) file to the top-level
        directory. A good README describes what the package does,
        how to install it, and includes a short example.',

  check = function(state) {
    readme_patterns <- c(
      "README.md", "README.Rmd", "README.qmd", "README"
    )
    found <- any(file.exists(file.path(state$path, readme_patterns)))
    list(status = found, positions = list())
  }
)

## --------------------------------------------------------------------

CHECKS$has_news <- make_check(

  description = "NEWS file exists",
  tags = c("info", "documentation"),
  preps = "package_structure",

  gp = 'add a {.file NEWS.md} file to track user-visible changes between
        releases. See {.url https://style.tidyverse.org/news.html}
        for formatting guidance.',

  check = function(state) {
    news_patterns <- c("NEWS.md", "NEWS", "NEWS.Rd", "inst/NEWS.Rd")
    found <- any(file.exists(file.path(state$path, news_patterns)))
    list(status = found, positions = list())
  }
)

## --------------------------------------------------------------------

CHECKS$r_file_extension <- make_check(

  description = "R scripts use .R file extension, not .r or .q",
  tags = c("CRAN", "warning"),
  preps = "package_structure",

  gp = paste(
    "use the {.file .R} file extension for R scripts,",
    "not {.file .r} or {.file .q}.",
    "CRAN requires the uppercase {.file .R} extension."
  ),

  check = function(state) {
    path <- state$path
    rdir <- file.path(path, "R")

    if (!dir.exists(rdir)) {
      return(list(status = TRUE, positions = list()))
    }

    files <- list.files(rdir, pattern = "\\.(r|q)$", ignore.case = FALSE)

    if (length(files) == 0) {
      list(status = TRUE, positions = list())
    } else {
      problems <- lapply(files, function(f) {
        list(
          filename = file.path("R", f),
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = f
        )
      })
      list(status = FALSE, positions = problems)
    }
  }
)
