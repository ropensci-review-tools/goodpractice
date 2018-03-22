
context("lintr")

test_that("library/require is OK in vignettes and examples", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "lintr_library_require_linter")
  df <- results(x)
  expect_identical(df$result, TRUE)
})

gp_bad2 <- gp("bad2", 
              checks = c("lintr_assignment_linter", "lintr_line_length_linter", 
                         "lintr_trailing_semicolon_linter", 
                         "lintr_attach_detach_linter", "lintr_setwd_linter", 
                         "lintr_sapply_linter", "lintr_library_require_linter", 
                         "lintr_seq_linter"))
res_bad2 <- results(gp_bad2)
get_result <- function(res, check) res$result[res$check == check]

test_that("assignment operator is <-", {
  expect_true(get_result(res_bad2, "lintr_assignment_linter"))
})

test_that("line length < 80", {
  expect_true(get_result(res_bad2, "lintr_line_length_linter"))
})

test_that("no trailing semicolon", {
  expect_true(get_result(res_bad2, "lintr_trailing_semicolon_linter"))
})

test_that("attach/detach is not okay", {
  expect_false(get_result(res_bad2, "lintr_attach_detach_linter"))
})

test_that("setwd is not okay", {
  expect_false(get_result(res_bad2, "lintr_setwd_linter"))
})

test_that("sapply is not okay", {
  expect_false(get_result(res_bad2, "lintr_sapply_linter"))
})

test_that("library/require is not okay in functions", {
  expect_false(get_result(res_bad2, "lintr_library_require_linter"))
})

test_that("no seq", {
  expect_true(get_result(res_bad2, "lintr_seq_linter"))
})
