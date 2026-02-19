
#' @include lists.R
#' @importFrom tools checkTnF

CHECKS$has_readme <- make_check(

  description = "README file exists",
  tags = c("info", "documentation"),
  preps = character(),

  gp = 'add a README.md (or README.Rmd) file to the top-level
        directory. A good README describes what the package does,
        how to install it, and includes a short example.',

  check = function(state) {
    readme_patterns <- c(
      "README.md", "README.Rmd", "README.qmd", "README"
    )
    any(file.exists(file.path(state$path, readme_patterns)))
  }
)

## --------------------------------------------------------------------

CHECKS$has_news <- make_check(

  description = "NEWS file exists",
  tags = c("info", "documentation"),
  preps = character(),

  gp = 'add a NEWS.md file to track user-visible changes between
        releases. See <https://style.tidyverse.org/news.html>
        for formatting guidance.',

  check = function(state) {
    news_patterns <- c("NEWS.md", "NEWS", "NEWS.Rd", "inst/NEWS.Rd")
    any(file.exists(file.path(state$path, news_patterns)))
  }
)

## --------------------------------------------------------------------

CHECKS$truefalse_not_tf <- make_check(

  description = "TRUE and FALSE is used, not T and F",
  tags = "warning",
  preps = character(),

  gp = "avoid 'T' and 'F', as they are just variables which are set to the
        logicals 'TRUE' and 'FALSE' by default, but are not reserved words
        and hence can be overwritten by the user.  Hence, one should
        always use 'TRUE' and 'FALSE' for the logicals.",

  check = function(state) {
    tf <- tryCatch(
      checkTnF(dir = state$path),
      error = function(e) list()
    )

    if (length(tf) == 0) return(list(status = TRUE, positions = list()))

    absfilenames <- rep(names(tf), vapply(tf, length, 1L))
    filenames <- file.path(
      basename(dirname(absfilenames)),
      basename(absfilenames)
    )

    positions <- lapply(filenames, function(x) {
      list(
        filename = x,
        line_number = NA_integer_,
        column_number = NA_integer_,
        ranges = list(),
        line = NA_character_
      )
    })

    list(
      status = FALSE,
      positions = positions
    )
  }
)

## --------------------------------------------------------------------

CHECKS$r_file_extension <- make_check(

  description = "R scripts use .R file extension, not .r or .q",
  tags = c("CRAN", "warning"),
  preps = character(),

  gp = paste(
    "use the .R file extension for R scripts, not .r or .q.",
    "CRAN requires the uppercase .R extension."
  ),

  check = function(state) {
    path <- state$path
    rdir <- file.path(path, "R")

    if (!dir.exists(rdir)) {
      return(list(status = TRUE, positions = list()))
    }

    files <- list.files(rdir, pattern = "\\.(r|q)$")

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
