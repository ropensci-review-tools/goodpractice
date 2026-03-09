
#' @include treesitter.R

get_tidyverse_lintr_position <- function(lint) {
  lint[c("filename", "line_number", "column_number", "ranges", "line")]
}

get_tidyverse_lintr_state <- function(state, linter) {
  if (inherits(state$tidyverse_lintr, "try-error")) {
    return(list(status = NA, positions = list()))
  }

  linters <- vapply(state$tidyverse_lintr, "[[", "", "linter")
  list(
    status = !linter %in% linters,
    positions = lapply(
      state$tidyverse_lintr[linters == linter],
      get_tidyverse_lintr_position
    )
  )
}

## --------------------------------------------------------------------
## Lintr-based tidyverse style checks
## --------------------------------------------------------------------

CHECKS$tidyverse_brace_linter <- make_check(

  description = "Opening brace on same line, closing brace on its own line",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'place the opening brace `{` on the same line as the
        statement and the closing brace `}` on its own line.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "brace_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_commas_linter <- make_check(

  description = "Space after commas, not before",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'place a space after a comma, never before, just like in
        regular English.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "commas_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_commented_code_linter <- make_check(

  description = "No commented-out code",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'remove commented-out code. Use version control to track
        old code instead of commenting it out.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "commented_code_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_equals_na_linter <- make_check(

  description = "Use is.na() not == NA",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'use `is.na(x)` instead of `x == NA`. Comparing with `==`
        always returns `NA`, never `TRUE` or `FALSE`.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "equals_na_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_function_left_parentheses_linter <- make_check(

  description = "No space before ( in function calls",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'not put a space before the opening parenthesis in a
        function call. Write `f(x)` not `f (x)`.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "function_left_parentheses_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_indentation_linter <- make_check(

  description = "Use 2-space indentation",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'use two spaces for indentation. Do not use tabs.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "indentation_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_infix_spaces_linter <- make_check(

  description = "Spaces around infix operators",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'put spaces around infix operators like `+`, `-`, `<-`,
        `==`, etc. Exception: no spaces around `::` and `:::`.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "infix_spaces_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_object_length_linter <- make_check(

  description = "Object names are not too long",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'keep object names concise. Strive for names that are
        concise and meaningful.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "object_length_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_object_name_linter <- make_check(

  description = "Object names use snake_case",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'use snake_case for variable and function names. Avoid
        camelCase, PascalCase, and dot.case.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "object_name_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_object_usage_linter <- make_check(

  description = "No unused variables or undefined globals",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'avoid defining variables that are never used and ensure
        all referenced objects are defined or imported.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "object_usage_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_paren_body_linter <- make_check(

  description = "No space after opening parenthesis",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'not put a space after `(` or before `)`. Write `f(x)`
        not `f( x )`.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "paren_body_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_pipe_consistency_linter <- make_check(

  description = "Consistent pipe operator usage",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'use one pipe operator consistently throughout the package.
        Do not mix `%>%` and `|>` in the same codebase.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "pipe_consistency_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_pipe_continuation_linter <- make_check(

  description = "Pipe continuation on next line",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'place the pipe operator at the end of the line, not the
        beginning of the next line.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "pipe_continuation_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_quotes_linter <- make_check(

  description = "Use double quotes for strings",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'use double quotes `\"` for strings, not single quotes `\'`.
        The only exception is when the string contains double quotes.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "quotes_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_return_linter <- make_check(

  description = "Use implicit return, not explicit return()",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'rely on implicit return. Only use `return()` for early
        returns. The last expression in a function is automatically
        returned.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "return_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_spaces_inside_linter <- make_check(

  description = "No spaces inside parentheses or brackets",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'not put spaces inside parentheses or brackets. Write
        `x[1]` and `f(x)`, not `x[ 1 ]` or `f( x )`.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "spaces_inside_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_spaces_left_parentheses_linter <- make_check(

  description = "Space before ( in control flow",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'place a space before `(` when used with `if`, `for`,
        `while`, etc. Write `if (x)` not `if(x)`.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "spaces_left_parentheses_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_trailing_blank_lines_linter <- make_check(

  description = "No trailing blank lines",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'end files with a single newline, not multiple blank lines.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "trailing_blank_lines_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_trailing_whitespace_linter <- make_check(

  description = "No trailing whitespace",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'remove trailing whitespace from code lines.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "trailing_whitespace_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_vector_logic_linter <- make_check(

  description = "Use && and || not & and | in if conditions",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'use `&&` and `||` (scalar logical operators) in `if`
        conditions, not `&` and `|` (vector logical operators).',

  check = function(state) {
    get_tidyverse_lintr_state(state, "vector_logic_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_whitespace_linter <- make_check(

  description = "No tab characters in code",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = 'use spaces instead of tabs for indentation.',

  check = function(state) {
    get_tidyverse_lintr_state(state, "whitespace_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_assignment_linter <- make_check(

  description = "Use <- for assignment, not =",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = "use '<-' for assignment instead of '='. This is the
        standard convention in R and makes code easier to read.",

  check = function(state) {
    get_tidyverse_lintr_state(state, "assignment_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_line_length_linter <- make_check(

  description = "Code lines are not too long",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = "keep code lines to a reasonable length for readability.",

  check = function(state) {
    get_tidyverse_lintr_state(state, "line_length_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_semicolon_linter <- make_check(

  description = "No trailing semicolons",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = "omit trailing semicolons from code lines. They are not
        needed in R and most style guides forbid them.",

  check = function(state) {
    get_tidyverse_lintr_state(state, "semicolon_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_seq_linter <- make_check(

  description = "Use seq_len() or seq_along() instead of 1:length(...)",
  tags = c("warning", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = "avoid 1:length(...), 1:nrow(...), and similar expressions.
        They are error prone when the right hand side is zero.
        Use seq_len() or seq_along() instead.",

  check = function(state) {
    get_tidyverse_lintr_state(state, "seq_linter")
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_T_and_F_symbol_linter <- make_check(

  description = "Use TRUE and FALSE, not T and F",
  tags = c("style", "tidyverse"),
  preps = "tidyverse_lintr",

  gp = "use TRUE and FALSE instead of T and F. T and F are not
        reserved words and can be overwritten, leading to unexpected
        behaviour.",

  check = function(state) {
    get_tidyverse_lintr_state(state, "T_and_F_symbol_linter")
  }
)

## --------------------------------------------------------------------
## Non-lintr structural checks
## --------------------------------------------------------------------

CHECKS$tidyverse_r_file_names <- make_check(

  description = "R file names use snake_case",
  tags = c("style", "tidyverse"),
  preps = character(),

  gp = 'name R files using snake_case with a .R extension, e.g.
        `my_function.R`. Avoid capital letters, hyphens, and spaces.',

  check = function(state) {
    r_dir <- file.path(state$path, "R")
    if (!dir.exists(r_dir)) return(TRUE)

    r_files <- basename(list.files(r_dir, pattern = "\\.[Rr]$"))
    bad_pattern <- "[A-Z]|[- ]"
    bad_files <- r_files[grepl(bad_pattern, tools::file_path_sans_ext(r_files))]

    list(
      status = length(bad_files) == 0,
      positions = lapply(bad_files, function(f) {
        list(
          filename = file.path("R", f),
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = f
        )
      })
    )
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_test_file_names <- make_check(

  description = "Test files mirror R source files",
  tags = c("style", "tidyverse"),
  preps = character(),

  gp = 'name test files to mirror the R source file they test,
        e.g. `R/my_function.R` should have `tests/testthat/test-my_function.R`.',

  check = function(state) {
    r_dir <- file.path(state$path, "R")
    if (!dir.exists(r_dir)) return(TRUE)

    r_files <- tools::file_path_sans_ext(
      basename(list.files(r_dir, pattern = "\\.[Rr]$"))
    )
    if (length(r_files) == 0) return(TRUE)

    test_dir <- file.path(state$path, "tests", "testthat")
    test_files <- if (dir.exists(test_dir)) {
      list.files(test_dir, pattern = "^test-.*\\.[Rr]$")
    } else {
      character()
    }
    tested <- sub("^test-", "", tools::file_path_sans_ext(test_files))

    untested <- setdiff(r_files, tested)
    untested <- untested[!grepl("^(zzz|RcppExports|reexport)", untested)]

    list(
      status = length(untested) == 0,
      positions = lapply(untested, function(f) {
        list(
          filename = file.path("R", paste0(f, ".R")),
          line_number = NA_integer_,
          column_number = NA_integer_,
          ranges = list(),
          line = paste0("missing tests/testthat/test-", f, ".R")
        )
      })
    )
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_no_missing <- make_check(

  description = "Functions do not use missing() to check arguments",
  tags = c("warning", "tidyverse"),
  preps = character(),

  gp = paste(
    "avoid using missing() to check whether arguments were supplied.",
    "The tidyverse style guide recommends using NULL defaults with",
    "is.null() instead, as missing() makes functions difficult to",
    "call programmatically."
  ),

  check = function(state) {
    ts <- ts_get(state)
    funcs <- ts$functions
    missing_q <- treesitter::query(ts$language,
      "(call function: (identifier) @fn (#eq? @fn \"missing\"))"
    )
    problems <- list()

    for (fn in funcs) {
      if (ts_body_has_call(fn$fn_node, missing_q)) {
        problems[[length(problems) + 1]] <- list(
          filename = file.path("R", basename(fn$file)),
          line_number = fn$line,
          column_number = NA_integer_,
          ranges = list(),
          line = fn$name
        )
      }
    }

    if (length(problems) == 0) {
      list(status = TRUE, positions = list())
    } else {
      list(status = FALSE, positions = problems)
    }
  }
)

## --------------------------------------------------------------------

CHECKS$tidyverse_export_order <- make_check(

  description = "Exported functions are defined before internal helpers",
  tags = c("style", "tidyverse"),
  preps = "namespace",

  gp = "define exported (user-facing) functions before internal
        helper functions within each R source file.",

  check = function(state) {
    if (inherits(state$namespace, "try-error")) {
      return(list(status = NA, positions = list()))
    }

    ns <- state$namespace
    exported <- ns$exports

    if (nrow(ns$S3methods) > 0) {
      exported <- c(exported, paste0(ns$S3methods[, 1], ".", ns$S3methods[, 2]))
    }

    patterns <- ns$exportPatterns

    is_exported <- function(name) {
      if (name %in% exported) return(TRUE)
      for (p in patterns) {
        if (grepl(p, name)) return(TRUE)
      }
      FALSE
    }

    funcs <- ts_get(state)$functions
    if (length(funcs) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    by_file <- split(funcs, vapply(funcs, `[[`, "", "file"))
    problems <- list()

    for (file_funcs in by_file) {
      if (length(file_funcs) < 2) next

      is_exp <- vapply(file_funcs, function(fn) is_exported(fn$name), logical(1))
      if (!any(is_exp) || all(is_exp)) next

      last_export <- max(which(is_exp))

      for (j in seq_len(last_export - 1)) {
        if (!is_exp[j]) {
          fn <- file_funcs[[j]]
          problems[[length(problems) + 1]] <- list(
            filename = file.path("R", basename(fn$file)),
            line_number = fn$line,
            column_number = NA_integer_,
            ranges = list(),
            line = fn$name
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
