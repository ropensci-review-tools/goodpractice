#' @include lists.R

vignette_files <- function(path) {
  vigdir <- file.path(path, "vignettes")
  if (!dir.exists(vigdir)) return(character())
  list.files(vigdir, pattern = "\\.(Rmd|Rnw|qmd)$",
             full.names = TRUE, recursive = TRUE)
}

CHECKS$vignette_no_rm_list <- make_check(

  description = "Vignettes do not use rm(list = ls())",
  tags = c("best practice", "warning"),
  preps = character(),

  gp = paste(
    "do not use rm(list = ls()) in vignettes.",
    "Vignettes run in their own environment;",
    "clearing the workspace is unnecessary and confusing for users."
  ),

  check = function(state) {
    path <- state$path
    vfiles <- vignette_files(path)
    problems <- list()

    for (f in vfiles) {
      lines <- readLines(f, warn = FALSE)
      for (i in seq_along(lines)) {
        if (grepl("rm\\s*\\(\\s*list\\s*=\\s*ls\\s*\\(", lines[i])) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("vignettes", basename(f)),
            line_number = i,
            column_number = NA_integer_,
            ranges = list(),
            line = trimws(lines[i])
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

CHECKS$vignette_no_setwd <- make_check(

  description = "Vignettes do not use setwd()",
  tags = c("best practice", "warning"),
  preps = character(),

  gp = paste(
    "do not use setwd() in vignettes.",
    "Changing the working directory makes vignettes fragile",
    "and non-reproducible on other machines."
  ),

  check = function(state) {
    path <- state$path
    vfiles <- vignette_files(path)
    problems <- list()

    for (f in vfiles) {
      lines <- readLines(f, warn = FALSE)
      for (i in seq_along(lines)) {
        if (grepl("setwd\\s*\\(", lines[i])) {
          problems[[length(problems) + 1]] <- list(
            filename = file.path("vignettes", basename(f)),
            line_number = i,
            column_number = NA_integer_,
            ranges = list(),
            line = trimws(lines[i])
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
