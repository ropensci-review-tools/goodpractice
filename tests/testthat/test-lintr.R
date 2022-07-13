
# "lintr_trailing_semicolon_linter" fails in bad1
# "lintr_attach_detach_linter"  fails in bad2
# "lintr_setwd_linter"  fails in bad2
# "lintr_sapply_linter"  fails in bad2
# "lintr_library_require_linter" fails in bad2
# "lintr_seq_linter" fails in bad3
# not failing yet:
# "lintr_assignment_linter"
# "lintr_line_length_linter"

gp_lintrs <- c("lintr_assignment_linter", "lintr_line_length_linter",
               "lintr_trailing_semicolon_linter",
               "lintr_attach_detach_linter", "lintr_setwd_linter",
               "lintr_sapply_linter", "lintr_library_require_linter",
               "lintr_seq_linter")

bad1 <- system.file("bad1", package = "goodpractice")
gp_bad1 <- gp(bad1, checks = gp_lintrs)
res_bad1 <- results(gp_bad1)

gp_bad2 <- gp("bad2", checks = gp_lintrs)
res_bad2 <- results(gp_bad2)

gp_bad3 <- gp("bad3", checks = gp_lintrs)
res_bad3 <- results(gp_bad3)

get_result <- function(res, check) res$result[res$check == check]



test_that("lintr_assignment_linter", {

  expect_true(get_result(res_bad1, "lintr_assignment_linter"))
  # TODO expectation/example where the check fails

})
test_that("lintr_line_length_linter", {

  expect_true(get_result(res_bad1, "lintr_line_length_linter"))
  # TODO expectation/example where the check fails

})
test_that("lintr_trailing_semicolon_linter", {

  expect_true(get_result(res_bad2, "lintr_trailing_semicolon_linter"))
  expect_false(get_result(res_bad1, "lintr_trailing_semicolon_linter"))

})
test_that("lintr_attach_detach_linter", {

  expect_true(get_result(res_bad1, "lintr_attach_detach_linter"))
  expect_false(get_result(res_bad2, "lintr_attach_detach_linter"))

})
test_that("lintr_setwd_linter", {

  expect_true(get_result(res_bad1, "lintr_setwd_linter"))
  expect_false(get_result(res_bad2, "lintr_setwd_linter"))

})
test_that("lintr_sapply_linter", {

  expect_true(get_result(res_bad1, "lintr_sapply_linter"))
  expect_false(get_result(res_bad2, "lintr_sapply_linter"))

})
test_that("lintr_library_require_linter", {

  # library/require is OK in vignettes and examples
  expect_true(get_result(res_bad1, "lintr_library_require_linter"))
  # but not in the R/ folder
  expect_false(get_result(res_bad2, "lintr_library_require_linter"))

})
test_that("lintr_seq_linter", {

  expect_true(get_result(res_bad1, "lintr_seq_linter"))
  expect_false(get_result(res_bad3, "lintr_seq_linter"))

})
