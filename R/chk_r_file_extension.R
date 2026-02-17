#' @include lists.R

CHECKS$r_file_extension <- make_check(

  description = "R scripts use .R file extension, not .r",
  tags = c("CRAN", "warning"),
  preps = character(),

  gp = paste(
    "use the .R file extension for R scripts, not .r.",
    "CRAN requires the uppercase .R extension."
  ),

  check = function(state) {
    path <- state$path
    rdir <- file.path(path, "R")

    if (!dir.exists(rdir)) {
      return(list(status = TRUE, positions = list()))
    }

    files <- list.files(rdir, pattern = "\\.[r]$")

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
