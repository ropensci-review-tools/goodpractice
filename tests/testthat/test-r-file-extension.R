get_result <- function(res, check) res$result[res$check == check]

test_that("r_file_extension fails when .r or .q files exist", {
  gp_res <- gp("bad_r_ext", checks = "r_file_extension")
  res <- results(gp_res)
  expect_false(get_result(res, "r_file_extension"))

  pos <- failed_positions(gp_res)$r_file_extension
  filenames <- vapply(pos, `[[`, "", "filename")
  expect_true(any(grepl("bad_func\\.r$", filenames)))
  expect_true(any(grepl("old_syntax\\.q$", filenames)))
})

test_that("r_file_extension passes when only .R files exist", {
  gp_res <- gp("good", checks = "r_file_extension")
  res <- results(gp_res)
  expect_true(get_result(res, "r_file_extension"))
})

test_that("r_file_extension passes when no R directory exists", {
  state <- list(path = tempdir())
  result <- CHECKS$r_file_extension$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})
