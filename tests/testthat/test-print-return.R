get_result <- function(res, check) res$result[res$check == check]

test_that("print_return_invisible fails when print method lacks invisible()", {
  gp_res <- gp("bad_print", checks = "print_return_invisible")
  res <- results(gp_res)
  expect_false(get_result(res, "print_return_invisible"))

  pos <- failed_positions(gp_res)$print_return_invisible
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("print\\.myclass", lines)))
})

test_that("print_return_invisible passes when no print methods exist", {
  gp_res <- gp("good", checks = "print_return_invisible")
  res <- results(gp_res)
  expect_true(get_result(res, "print_return_invisible"))
})

test_that("print_return_invisible passes when no R directory exists", {
  state <- list(path = tempdir())
  result <- CHECKS$print_return_invisible$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})
