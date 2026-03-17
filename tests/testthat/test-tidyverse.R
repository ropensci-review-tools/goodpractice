
tv_checks <- tidyverse_checks()
tv_lintr_checks <- grep("_linter$", tv_checks, value = TRUE)

get_result <- function(res, check) res$passed[res$check == check]

gp_bad <- gp("bad_tidyverse", checks = tv_checks)
res_bad <- results(gp_bad)

gp_good <- gp("good_tidyverse", checks = tv_checks)
res_good <- results(gp_good)

test_that("tidyverse_checks() returns only tidyverse_ prefixed checks", {
  expect_true(length(tv_checks) > 0)
  expect_true(all(grepl("^tidyverse_", tv_checks)))
})

test_that("default_checks() excludes tidyverse checks", {
  dc <- default_checks()
  expect_false(any(grepl("^tidyverse_", dc)))
  expect_equal(sort(c(dc, tv_checks)), sort(all_checks()))
})

test_that("tidyverse lintr checks pass on good fixture", {
  for (chk in tv_lintr_checks) {
    expect_true(
      get_result(res_good, chk),
      label = paste(chk, "should pass on good_tidyverse")
    )
  }
})

test_that("tidyverse brace_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_brace_linter"))
})

test_that("tidyverse commas_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_commas_linter"))
})

test_that("tidyverse commented_code_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_commented_code_linter"))
})

test_that("tidyverse equals_na_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_equals_na_linter"))
})

test_that("tidyverse function_left_parentheses_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_function_left_parentheses_linter"))
})

test_that("tidyverse indentation_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_indentation_linter"))
})

test_that("tidyverse infix_spaces_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_infix_spaces_linter"))
})

test_that("tidyverse object_length_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_object_length_linter"))
})

test_that("tidyverse object_name_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_object_name_linter"))
})

test_that("tidyverse object_usage_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_object_usage_linter"))
})

test_that("tidyverse paren_body_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_paren_body_linter"))
})

test_that("tidyverse pipe_consistency_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_pipe_consistency_linter"))
})

test_that("tidyverse quotes_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_quotes_linter"))
})

test_that("tidyverse return_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_return_linter"))
})

test_that("tidyverse spaces_inside_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_spaces_inside_linter"))
})

test_that("tidyverse spaces_left_parentheses_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_spaces_left_parentheses_linter"))
})

test_that("tidyverse trailing_blank_lines_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_trailing_blank_lines_linter"))
})

test_that("tidyverse trailing_whitespace_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_trailing_whitespace_linter"))
})

test_that("tidyverse vector_logic_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_vector_logic_linter"))
})

test_that("tidyverse whitespace_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_whitespace_linter"))
})

test_that("tidyverse assignment_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_assignment_linter"))
})

test_that("tidyverse line_length_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_line_length_linter"))
})

test_that("tidyverse semicolon_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_semicolon_linter"))
})

test_that("tidyverse seq_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_seq_linter"))
})

test_that("tidyverse T_and_F_symbol_linter fails on bad fixture", {
  expect_false(get_result(res_bad, "tidyverse_T_and_F_symbol_linter"))
})

test_that("tidyverse_r_file_names catches bad file names", {
  expect_false(get_result(res_bad, "tidyverse_r_file_names"))
  expect_true(get_result(res_good, "tidyverse_r_file_names"))
})

test_that("tidyverse_test_file_names catches missing test files", {
  expect_false(get_result(res_bad, "tidyverse_test_file_names"))
  expect_true(get_result(res_good, "tidyverse_test_file_names"))
})

test_that("tidyverse_no_missing fails when missing() is used", {
  gp_res <- gp("bad_missing", checks = "tidyverse_no_missing")
  res <- results(gp_res)
  expect_false(get_result(res, "tidyverse_no_missing"))

  pos <- failed_positions(gp_res)$tidyverse_no_missing
  names <- vapply(pos, `[[`, "", "line")
  expect_true("my_func" %in% names)
})

test_that("tidyverse_no_missing passes when missing() is not used", {
  expect_true(get_result(res_good, "tidyverse_no_missing"))
})

test_that("tidyverse_export_order fails when internal before exported", {
  gp_res <- gp("bad_export_order", checks = "tidyverse_export_order")
  res <- results(gp_res)
  expect_false(get_result(res, "tidyverse_export_order"))

  pos <- failed_positions(gp_res)$tidyverse_export_order
  names <- vapply(pos, `[[`, "", "line")
  expect_true("internal_helper" %in% names)
})

test_that("tidyverse_export_order passes with exportPattern", {
  expect_true(get_result(res_good, "tidyverse_export_order"))
})

test_that("tidyverse lintr checks respect .lintr config", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("bad_tidyverse", full.names = TRUE, recursive = TRUE),
    pkg, recursive = TRUE
  )
  writeLines(
    "linters: linters_with_defaults(brace_linter = NULL)",
    file.path(pkg, ".lintr")
  )

  gp_res <- gp(pkg, checks = "tidyverse_brace_linter")
  res <- results(gp_res)
  expect_true(res$passed[res$check == "tidyverse_brace_linter"])
})

test_that("get_tidyverse_lintr_state returns NA on try-error", {
  state <- list(tidyverse_lintr = structure("error", class = "try-error"))
  result <- get_tidyverse_lintr_state(state, "brace_linter")
  expect_true(is.na(result$status))
  expect_equal(result$positions, list())
})

test_that("tidyverse_no_missing ignores missing() inside nested functions", {
  pkg <- withr::local_tempdir()
  dir.create(file.path(pkg, "R"))
  writeLines(
    c(
      "Package: nesttest", "Title: Test", "Version: 1.0.0",
      "Author: Test", "Maintainer: Test <test@test.com>",
      "Description: Test.", "License: GPL-2"
    ),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(
    c(
      "outer <- function(x) {",
      "  inner <- function(y) missing(y)",
      "  inner(x)",
      "}"
    ),
    file.path(pkg, "R", "funcs.R")
  )

  gp_res <- gp(pkg, checks = "tidyverse_no_missing")
  res <- results(gp_res)
  expect_true(get_result(res, "tidyverse_no_missing"))
})

test_that("find_top_level_functions handles edge cases", {
  find_funcs <- find_top_level_functions
  pkg <- withr::local_tempdir()
  writeLines(
    c(
      "Package: edgetest", "Title: Test", "Version: 1.0.0",
      "Author: Test", "Maintainer: Test <test@test.com>",
      "Description: Test.", "License: GPL-2"
    ),
    file.path(pkg, "DESCRIPTION")
  )

  expect_equal(find_funcs(pkg), list())

  dir.create(file.path(pkg, "R"))
  writeLines(character(), file.path(pkg, "R", "empty.R"))
  expect_equal(find_funcs(pkg), list())

  writeLines(
    c("x <- 42", "library(stats)", "y <- sum(1:10)"),
    file.path(pkg, "R", "misc.R")
  )
  expect_equal(find_funcs(pkg), list())
})
