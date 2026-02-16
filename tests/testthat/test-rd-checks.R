get_result <- function(res, check) res$result[res$check == check]

# -- rd_has_examples ----------------------------------------------------------

test_that("rd_has_examples fails when exported function has no examples", {
  gp_res <- gp("bad_rd", checks = "rd_has_examples")
  res <- results(gp_res)
  expect_false(get_result(res, "rd_has_examples"))

  pos <- failed_positions(gp_res)$rd_has_examples
  lines <- vapply(pos, `[[`, "", "line")
  expect_true("no_examples" %in% lines)
})

test_that("rd_has_examples passes when all exports have examples", {
  gp_res <- gp("good", checks = "rd_has_examples")
  res <- results(gp_res)
  expect_true(get_result(res, "rd_has_examples"))
})

test_that("rd_has_examples skips S3 methods", {
  gp_res <- gp("bad_rd", checks = "rd_has_examples")
  pos <- failed_positions(gp_res)$rd_has_examples
  lines <- vapply(pos, `[[`, "", "line")
  expect_false("print.badrd" %in% lines)
})

# -- rd_has_return ------------------------------------------------------------

test_that("rd_has_return fails when exported function has no value", {
  gp_res <- gp("bad_rd", checks = "rd_has_return")
  res <- results(gp_res)
  expect_false(get_result(res, "rd_has_return"))

  pos <- failed_positions(gp_res)$rd_has_return
  lines <- vapply(pos, `[[`, "", "line")
  expect_true("no_value" %in% lines)
})

test_that("rd_has_return passes when all exports have value", {
  gp_res <- gp("good", checks = "rd_has_return")
  res <- results(gp_res)
  expect_true(get_result(res, "rd_has_return"))
})

# -- rd_examples_dontrun ------------------------------------------------------

test_that("rd_examples_dontrun fails when examples use dontrun", {
  gp_res <- gp("bad_rd", checks = "rd_examples_dontrun")
  res <- results(gp_res)
  expect_false(get_result(res, "rd_examples_dontrun"))

  pos <- failed_positions(gp_res)$rd_examples_dontrun
  filenames <- vapply(pos, `[[`, "", "filename")
  expect_true(any(grepl("dontrun_example", filenames)))
})

test_that("rd_examples_dontrun passes when no dontrun is used", {
  gp_res <- gp("good", checks = "rd_examples_dontrun")
  res <- results(gp_res)
  expect_true(get_result(res, "rd_examples_dontrun"))
})

# -- rd_examples_runnable -----------------------------------------------------

test_that("rd_examples_runnable fails when all code is wrapped", {
  gp_res <- gp("bad_rd", checks = "rd_examples_runnable")
  res <- results(gp_res)
  expect_false(get_result(res, "rd_examples_runnable"))

  pos <- failed_positions(gp_res)$rd_examples_runnable
  filenames <- vapply(pos, `[[`, "", "filename")
  expect_true(any(grepl("all_wrapped", filenames)))
})

test_that("rd_examples_runnable passes when runnable code exists", {
  gp_res <- gp("good", checks = "rd_examples_runnable")
  res <- results(gp_res)
  expect_true(get_result(res, "rd_examples_runnable"))
})

# -- prep returns NA on missing man/ ------------------------------------------

test_that("rd checks return NA when man/ directory is missing", {
  gp_res <- gp("bad_tags", checks = "rd_has_examples")
  res <- results(gp_res)
  expect_true(is.na(get_result(res, "rd_has_examples")))
})

# -- prep error handling ------------------------------------------------------

test_that("rd checks return NA when prep failed", {
  state <- list(rd = structure("error", class = "try-error"))
  result <- CHECKS$rd_has_examples$check(state)
  expect_true(is.na(result$status))
})
