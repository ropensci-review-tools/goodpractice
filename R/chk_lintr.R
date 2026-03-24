
#' @include treesitter.R

get_lintr_position <- function(linter) {
  linter[c("filename", "line_number", "column_number", "ranges", "line")]
}

get_lintr_state <- function(state, linter) {
  if(inherits(state$lintr, "try-error")) {
    return(list(status = NA, position = list()))
  }
  
  linters <- vapply(state$lintr, "[[", "", "linter")
  list(
    status = ! linter %in% linters,
    positions = lapply(
      state$lintr[linters == linter],
      get_lintr_position
    )
  )
}

make_lintr_check <- function(linter_name, description, gp, tags = "lintr") {
  make_check(
    description = description,
    tags = c("warning", tags),
    preps = "lintr",
    gp = gp,
    check = function(state) get_lintr_state(state, linter_name)
  )
}

#' @include lists.R

CHECKS$lintr_assignment_linter <- make_check(

  description = "'<-' and not '=' is used for assignment",
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "use '<-' for assignment instead of '='.
        '<-' is the standard, and R users and developers are
        used it and it is easier to read your code for them if
        you use '<-'.",

  check = function(state) {
    result <- get_lintr_state(state, "assignment_linter")
    filter_s4_assignment_false_positives(state, result)
  }
)

CHECKS$lintr_line_length_linter <- make_check(

  description = "Code lines are short",
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "avoid long code lines, it is bad for
        readability. Also, many people prefer editor windows
        that are about 80 characters wide. Try making your lines
        shorter than 80 characters",

  check = function(state) {
    get_lintr_state(state, "line_length_linter")
  }
)

CHECKS$lintr_semicolon_linter <- make_check(

  description = "No trailing semicolons",
  tags = c("style", "lintr"),
  preps = "lintr",

  gp = "omit trailing semicolons from code lines.
        They are not needed and most R coding standards
        forbid them",

  check = function(state) {
    get_lintr_state(state, "semicolon_linter")
  }
)

CHECKS$lintr_attach_detach_linter <- make_check(

  description = "Avoid attach and detach",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid the attach() and detach() functions,
        they are fragile and code that uses them will
        probably break sooner than later.",

  check = function(state) {
    get_lintr_state(state, "attach_detach_linter")
  }
)

CHECKS$lintr_setwd_linter <- make_check(

  description = "Avoid setwd in R packages",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid calling setwd(), it changes the global environment.
        If you need it, consider using on.exit() to restore the
        working directory.",

  check = function(state) {
    get_lintr_state(state, "setwd_linter")
  }
)

CHECKS$lintr_sapply_linter <- make_check(

  description = "Avoid sapply",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid sapply(), it is not type safe.
        It might return a vector, or a list, depending on the
        input data. Consider using vapply() instead.",

  check = function(state) {
    get_lintr_state(state, "sapply_linter")
  }
)

CHECKS$lintr_library_require_linter <- make_check(

  description = "Avoid library and require in packages",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid the library() and require() functions,
        they change the global search path.
        If you need to use other packages, import them.
        If you need to load them explicitly, then consider
        loadNamespace() instead, or as a last resort, declare
        them as 'Depends' dependencies.",

  check = function(state) {
    if(inherits(state$lintr, "try-error")) {
      return(list(status = NA, position = list()))
    }
    
    res <- get_lintr_state(state, "library_require_linter")

    ## library() and require() are OK in tests and vignettes
    res$positions <- Filter(
      f = function(x) grepl("^R[/(\\)]", x$filename),
      res$positions
    )
    res$status <- length(res$positions) == 0
    res
  }
)

CHECKS$lintr_seq_linter <- make_check(

  description = "Avoid 1:length(...) and similar expressions",
  tags = c("warning", "lintr"),
  preps = "lintr",

  gp = "avoid 1:length(...), 1:nrow(...), 1:ncol(...),
        1:NROW(...) and 1:NCOL(...) expressions. They are error
        prone and result 1:0 if the expression on the right hand
        side is zero. Use seq_len() or seq_along() instead.",

  check = function(state) {
    get_lintr_state(state, "seq_linter")
  }
)

## -- correctness / performance ------------------------------------------------

CHECKS$lintr_any_duplicated_linter <- make_lintr_check(
  "any_duplicated_linter",
  "Use anyDuplicated() instead of any(duplicated())",
  "use anyDuplicated(x) instead of any(duplicated(x)). It is more
        efficient because it stops at the first duplicate."
)

CHECKS$lintr_any_is_na_linter <- make_lintr_check(
  "any_is_na_linter",
  "Use anyNA() instead of any(is.na())",
  "use anyNA(x) instead of any(is.na(x)). It is more efficient
        because it stops at the first NA."
)

CHECKS$lintr_class_equals_linter <- make_lintr_check(
  "class_equals_linter",
  "Use inherits() instead of class() ==",
  "use inherits(x, 'class') instead of class(x) == 'class'.
        Objects can have multiple classes, so == may miss matches."
)

CHECKS$lintr_condition_message_linter <- make_lintr_check(
  "condition_message_linter",
  "Build condition messages correctly",
  "build error and warning messages with the condition function itself,
        not with paste() or sprintf(). Use stop('msg') not stop(paste('msg'))."
)

CHECKS$lintr_duplicate_argument_linter <- make_lintr_check(
  "duplicate_argument_linter",
  "No duplicate arguments in function calls",
  "avoid passing the same argument twice in a function call.
        Duplicate arguments are silently ignored, which is likely a bug."
)

CHECKS$lintr_equals_na_linter <- make_lintr_check(
  "equals_na_linter",
  "Use is.na() not == NA",
  "use is.na(x) instead of x == NA. Comparing with == always
        returns NA, never TRUE or FALSE."
)

CHECKS$lintr_fixed_regex_linter <- make_lintr_check(
  "fixed_regex_linter",
  "Use fixed strings instead of regex where possible",
  "use fixed = TRUE in grep/grepl/sub/gsub when the pattern is a
        plain string, not a regular expression. It is faster and clearer."
)

CHECKS$lintr_for_loop_index_linter <- make_lintr_check(
  "for_loop_index_linter",
  "Do not overwrite existing variables with for loop index",
  "avoid using a variable name in for() that already exists in the
        surrounding scope. This overwrites the existing value."
)

CHECKS$lintr_length_test_linter <- make_lintr_check(
  "length_test_linter",
  "Use length() correctly in conditions",
  "avoid length(x) == 0 or length(x) > 0 in if() conditions when
        a type-stable alternative like is.null() or nzchar() exists."
)

CHECKS$lintr_matrix_apply_linter <- make_lintr_check(
  "matrix_apply_linter",
  "Use colSums/rowSums/colMeans/rowMeans instead of apply()",
  "use colSums(), rowSums(), colMeans(), or rowMeans() instead of
        apply() with sum or mean. The specialized functions are much faster."
)

CHECKS$lintr_missing_argument_linter <- make_lintr_check(
  "missing_argument_linter",
  "No missing arguments in function calls",
  "avoid empty arguments in function calls like f(x, , y).
        Missing arguments are a common source of bugs."
)

CHECKS$lintr_nrow_subset_linter <- make_lintr_check(
  "nrow_subset_linter",
  "Use nrow(subset) instead of nrow() after subsetting",
  "avoid patterns like nrow(x[condition, ]). Subset first, then
        count rows, or use sum(condition) directly."
)

CHECKS$lintr_redundant_equals_linter <- make_lintr_check(
  "redundant_equals_linter",
  "No redundant comparisons to TRUE or FALSE",
  "avoid x == TRUE or x == FALSE. Use x or !x directly. The
        comparison is redundant and less readable."
)

CHECKS$lintr_redundant_ifelse_linter <- make_lintr_check(
  "redundant_ifelse_linter",
  "No redundant ifelse() calls",
  "avoid ifelse(condition, TRUE, FALSE) or similar. The condition
        itself is already logical - use it directly."
)

CHECKS$lintr_regex_subset_linter <- make_lintr_check(
  "regex_subset_linter",
  "Use grep() with value = TRUE instead of subsetting",
  "use grep(pattern, x, value = TRUE) instead of
        x[grep(pattern, x)] or x[grepl(pattern, x)]."
)

CHECKS$lintr_sort_linter <- make_lintr_check(
  "sort_linter",
  "Use sort() or is.unsorted() instead of order()",
  "use sort(x) instead of x[order(x)] and is.unsorted() instead
        of checking all(x == sort(x)). The dedicated functions are clearer."
)

CHECKS$lintr_system_file_linter <- make_lintr_check(
  "system_file_linter",
  "Use system.file() correctly",
  "use system.file('dir', 'file', package = 'pkg') instead of
        file.path(system.file(package = 'pkg'), 'dir', 'file'). The
        single-call form correctly returns '' when the file does not exist."
)

CHECKS$lintr_terminal_close_linter <- make_lintr_check(
  "terminal_close_linter",
  "Do not close connections redundantly",
  "avoid closing connections at the end of a function when they
        will be closed automatically. Use on.exit(close(con)) for
        connections that need explicit cleanup."
)

CHECKS$lintr_which_grepl_linter <- make_lintr_check(
  "which_grepl_linter",
  "Use grep() instead of which(grepl())",
  "use grep(pattern, x) instead of which(grepl(pattern, x)).
        grep() returns indices directly."
)

## -- readability / idiom ------------------------------------------------------

CHECKS$lintr_boolean_arithmetic_linter <- make_lintr_check(
  "boolean_arithmetic_linter",
  "Avoid arithmetic on logical vectors",
  "avoid sum(x == value) or similar arithmetic on logical vectors.
        Use sum() with a logical condition only when counting, and prefer
        dedicated functions like any() or all() for logical tests."
)

CHECKS$lintr_comparison_negation_linter <- make_lintr_check(
  "comparison_negation_linter",
  "Use natural comparison operators instead of negation",
  "use x != y instead of !(x == y), and x >= y instead of
        !(x < y). Direct comparisons are clearer."
)

CHECKS$lintr_consecutive_assertion_linter <- make_lintr_check(
  "consecutive_assertion_linter",
  "Combine consecutive stopifnot() calls",
  "combine consecutive stopifnot() calls into one. Multiple
        conditions can be passed to a single stopifnot()."
)

CHECKS$lintr_if_not_else_linter <- make_lintr_check(
  "if_not_else_linter",
  "Avoid if (!cond) { a } else { b }",
  "prefer if (cond) { b } else { a } over if (!cond) { a } else { b }.
        Positive conditions are easier to reason about."
)

CHECKS$lintr_if_switch_linter <- make_lintr_check(
  "if_switch_linter",
  "Use switch() instead of long if/else chains",
  "consider using switch() instead of long if/else if chains that
        compare the same variable to different values."
)

CHECKS$lintr_ifelse_censor_linter <- make_lintr_check(
  "ifelse_censor_linter",
  "Use pmin()/pmax() instead of ifelse() for clamping",
  "use pmin() or pmax() instead of ifelse(x > limit, limit, x).
        The dedicated functions are faster and clearer for clamping values."
)

CHECKS$lintr_implicit_assignment_linter <- make_lintr_check(
  "implicit_assignment_linter",
  "Avoid implicit assignments in function calls",
  "avoid assignments inside function calls like f(x <- value).
        Assign first, then call the function. Implicit assignments are
        easy to miss when reading code."
)

CHECKS$lintr_inner_combine_linter <- make_lintr_check(
  "inner_combine_linter",
  "Combine inside c() not outside",
  "use c(paste0('a', x), paste0('b', x)) instead of
        paste0(c('a', 'b'), x). Combining at the outer level is clearer."
)

CHECKS$lintr_length_levels_linter <- make_lintr_check(
  "length_levels_linter",
  "Use nlevels() instead of length(levels())",
  "use nlevels(x) instead of length(levels(x)). nlevels() is
        more expressive and works correctly with non-factor inputs."
)

CHECKS$lintr_literal_coercion_linter <- make_lintr_check(
  "literal_coercion_linter",
  "Use typed literals instead of coercion functions",
  "use 1L instead of as.integer(1), and 'text' instead of
        as.character('text'). Typed literals are clearer and faster."
)

CHECKS$lintr_nested_ifelse_linter <- make_lintr_check(
  "nested_ifelse_linter",
  "Avoid deeply nested ifelse() calls",
  "avoid nesting ifelse() calls. Use dplyr::case_when() or
        data.table::fcase() for multi-condition logic, or a lookup table."
)

CHECKS$lintr_nested_pipe_linter <- make_lintr_check(
  "nested_pipe_linter",
  "Avoid pipes inside other pipes",
  "avoid nesting pipe chains inside other pipe chains. Extract the
        inner pipeline into a named intermediate variable."
)

CHECKS$lintr_numeric_leading_zero_linter <- make_lintr_check(
  "numeric_leading_zero_linter",
  "Include leading zero in decimal numbers",
  "write 0.1 instead of .1. The leading zero makes decimal numbers
        easier to spot when reading code."
)

CHECKS$lintr_outer_negation_linter <- make_lintr_check(
  "outer_negation_linter",
  "Negate at the outer level",
  "use !any(x) instead of all(!x), and !all(x) instead of
        any(!x). Outer negation short-circuits and is clearer."
)

CHECKS$lintr_paste_linter <- make_lintr_check(
  "paste_linter",
  "Use paste0() or file.path() instead of paste() with sep",
  "use paste0() instead of paste(..., sep = '') and
        file.path() instead of paste(..., sep = '/')."
)

CHECKS$lintr_scalar_in_linter <- make_lintr_check(
  "scalar_in_linter",
  "Use == instead of %in% for scalar comparison",
  "use x == 'value' instead of x %in% 'value' when comparing
        against a single scalar. == is clearer for single values."
)

CHECKS$lintr_strings_as_factors_linter <- make_lintr_check(
  "strings_as_factors_linter",
  "Do not use stringsAsFactors argument",
  "remove stringsAsFactors arguments. Since R 4.0, the default is
        FALSE. Specifying it is no longer needed."
)

CHECKS$lintr_undesirable_operator_linter <- make_lintr_check(
  "undesirable_operator_linter",
  "Avoid undesirable operators",
  "avoid operators like <<- (global assignment) and :::
        (accessing internal package functions). These make code harder
        to understand and maintain."
)

CHECKS$lintr_unnecessary_concatenation_linter <- make_lintr_check(
  "unnecessary_concatenation_linter",
  "Avoid unnecessary c() calls",
  "avoid c() with a single argument or no arguments. c('x') is
        just 'x', and c() is better written as NULL."
)

CHECKS$lintr_unnecessary_lambda_linter <- make_lintr_check(
  "unnecessary_lambda_linter",
  "Avoid unnecessary anonymous functions",
  "avoid \\(x) f(x) when f alone can be passed directly.
        Unnecessary wrappers add noise without changing behaviour."
)

CHECKS$lintr_unreachable_code_linter <- make_lintr_check(
  "unreachable_code_linter",
  "No unreachable code after return/stop/next",
  "remove code after return(), stop(), or next statements.
        Unreachable code is dead code and likely a bug."
)

## -- testthat expectations ---------------------------------------------------

CHECKS$lintr_conjunct_test_linter <- make_lintr_check(
  "conjunct_test_linter",
  "Use separate expect_ calls instead of &&",
  paste(
    "split expect_true(a && b) into separate expect_true(a) and",
    "expect_true(b) calls. Separate assertions give clearer failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_comparison_linter <- make_lintr_check(
  "expect_comparison_linter",
  "Use expect_gt/lt/gte/lte instead of expect_true with comparison",
  paste(
    "use expect_gt(x, y) instead of expect_true(x > y). Dedicated",
    "comparison expectations give better failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_identical_linter <- make_lintr_check(
  "expect_identical_linter",
  "Use expect_identical() instead of expect_equal() where possible",
  paste(
    "prefer expect_identical() over expect_equal() when the comparison",
    "should be exact. expect_equal() uses all.equal() with tolerance,",
    "which can mask subtle type or attribute differences."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_length_linter <- make_lintr_check(
  "expect_length_linter",
  "Use expect_length() instead of expect_equal(length())",
  paste(
    "use expect_length(x, n) instead of expect_equal(length(x), n).",
    "The dedicated function gives clearer failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_named_linter <- make_lintr_check(
  "expect_named_linter",
  "Use expect_named() instead of expect_equal(names())",
  paste(
    "use expect_named(x, expected) instead of",
    "expect_equal(names(x), expected). The dedicated function gives",
    "clearer failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_not_linter <- make_lintr_check(
  "expect_not_linter",
  "Use expect_false() instead of expect_true(!x)",
  paste(
    "use expect_false(x) instead of expect_true(!x). The dedicated",
    "function gives clearer failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_null_linter <- make_lintr_check(
  "expect_null_linter",
  "Use expect_null() instead of expect_equal(x, NULL)",
  paste(
    "use expect_null(x) instead of expect_equal(x, NULL) or",
    "expect_identical(x, NULL). The dedicated function gives clearer",
    "failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_s3_class_linter <- make_lintr_check(
  "expect_s3_class_linter",
  "Use expect_s3_class() instead of expect_equal(class())",
  paste(
    "use expect_s3_class(x, 'class') instead of",
    "expect_equal(class(x), 'class'). The dedicated function gives",
    "clearer failure messages and handles multiple classes correctly."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_s4_class_linter <- make_lintr_check(
  "expect_s4_class_linter",
  "Use expect_s4_class() instead of expect_true(is())",
  paste(
    "use expect_s4_class(x, 'Class') instead of",
    "expect_true(is(x, 'Class')). The dedicated function gives",
    "clearer failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_true_false_linter <- make_lintr_check(
  "expect_true_false_linter",
  "Use expect_true()/expect_false() instead of expect_equal(x, TRUE)",
  paste(
    "use expect_true(x) instead of expect_equal(x, TRUE) and",
    "expect_false(x) instead of expect_equal(x, FALSE). The dedicated",
    "functions give clearer failure messages."
  ),
  tags = c("lintr", "testing")
)

CHECKS$lintr_expect_type_linter <- make_lintr_check(
  "expect_type_linter",
  "Use expect_type() instead of expect_equal(typeof())",
  paste(
    "use expect_type(x, 'type') instead of",
    "expect_equal(typeof(x), 'type'). The dedicated function gives",
    "clearer failure messages."
  ),
  tags = c("lintr", "testing")
)
