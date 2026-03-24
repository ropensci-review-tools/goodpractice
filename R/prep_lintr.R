
linters_to_lint <- list(
  # -- existing checks --
  assignment_linter = lintr::assignment_linter(),
  line_length_linter = lintr::line_length_linter(80),
  package_hooks_linter = lintr::package_hooks_linter(),
  semicolon_linter = lintr::semicolon_linter(allow_compound = TRUE),
  attach_detach_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions[c("attach", "detach")]
  ),
  setwd_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions["setwd"]
  ),
  sapply_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions["sapply"]
  ),
  library_require_linter = lintr::undesirable_function_linter(
    fun = lintr::default_undesirable_functions[c("library", "require")]
  ),
  seq_linter = lintr::seq_linter(),

  # -- correctness / performance --
  any_duplicated_linter = lintr::any_duplicated_linter(),
  any_is_na_linter = lintr::any_is_na_linter(),
  class_equals_linter = lintr::class_equals_linter(),
  condition_message_linter = lintr::condition_message_linter(),
  duplicate_argument_linter = lintr::duplicate_argument_linter(),
  equals_na_linter = lintr::equals_na_linter(),
  fixed_regex_linter = lintr::fixed_regex_linter(),
  for_loop_index_linter = lintr::for_loop_index_linter(),
  length_test_linter = lintr::length_test_linter(),
  matrix_apply_linter = lintr::matrix_apply_linter(),
  missing_argument_linter = lintr::missing_argument_linter(),
  nrow_subset_linter = lintr::nrow_subset_linter(),
  redundant_equals_linter = lintr::redundant_equals_linter(),
  redundant_ifelse_linter = lintr::redundant_ifelse_linter(),
  regex_subset_linter = lintr::regex_subset_linter(),
  sort_linter = lintr::sort_linter(),
  system_file_linter = lintr::system_file_linter(),
  terminal_close_linter = lintr::terminal_close_linter(),
  which_grepl_linter = lintr::which_grepl_linter(),

  # -- readability / idiom --
  boolean_arithmetic_linter = lintr::boolean_arithmetic_linter(),
  comparison_negation_linter = lintr::comparison_negation_linter(),
  consecutive_assertion_linter = lintr::consecutive_assertion_linter(),
  if_not_else_linter = lintr::if_not_else_linter(),
  if_switch_linter = lintr::if_switch_linter(
    max_branch_lines = 2L, max_branch_expressions = 2L
  ),
  ifelse_censor_linter = lintr::ifelse_censor_linter(),
  implicit_assignment_linter = lintr::implicit_assignment_linter(),
  inner_combine_linter = lintr::inner_combine_linter(),
  length_levels_linter = lintr::length_levels_linter(),
  literal_coercion_linter = lintr::literal_coercion_linter(),
  nested_ifelse_linter = lintr::nested_ifelse_linter(),
  nested_pipe_linter = lintr::nested_pipe_linter(),
  numeric_leading_zero_linter = lintr::numeric_leading_zero_linter(),
  outer_negation_linter = lintr::outer_negation_linter(),
  paste_linter = lintr::paste_linter(),
  scalar_in_linter = lintr::scalar_in_linter(),
  strings_as_factors_linter = lintr::strings_as_factors_linter(),
  undesirable_operator_linter = lintr::undesirable_operator_linter(),
  unnecessary_concatenation_linter = lintr::unnecessary_concatenation_linter(),
  unnecessary_lambda_linter = lintr::unnecessary_lambda_linter(),
  unreachable_code_linter = lintr::unreachable_code_linter(),

  # -- testthat expectations --
  conjunct_test_linter = lintr::conjunct_test_linter(),
  expect_comparison_linter = lintr::expect_comparison_linter(),
  expect_identical_linter = lintr::expect_identical_linter(),
  expect_length_linter = lintr::expect_length_linter(),
  expect_named_linter = lintr::expect_named_linter(),
  expect_not_linter = lintr::expect_not_linter(),
  expect_null_linter = lintr::expect_null_linter(),
  expect_s3_class_linter = lintr::expect_s3_class_linter(),
  expect_s4_class_linter = lintr::expect_s4_class_linter(),
  expect_true_false_linter = lintr::expect_true_false_linter(),
  expect_type_linter = lintr::expect_type_linter()
)

#' @include lists.R prep_utils.R
#' @importFrom lintr lint_package

PREPS$lintr <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)
  run_prep_step(state, "lintr", function(path) {
    suppressMessages(lint_package(path, linters = linters_to_lint))
  }, path = path, silent = quiet)
}
