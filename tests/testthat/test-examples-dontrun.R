
get_result <- function(res, check) res$result[res$check == check]

# -- examples_dontrun ---------------------------------------------------------

test_that("examples_dontrun fails when examples use \\dontrun", {
  gp_res <- gp("bad_examples", checks = "examples_dontrun")
  res <- results(gp_res)
  expect_false(get_result(res, "examples_dontrun"))

  pos <- failed_positions(gp_res)$examples_dontrun
  expect_length(pos, 1)
})

test_that("examples_dontrun passes when no \\dontrun is used", {
  gp_res <- gp("good", checks = "examples_dontrun")
  res <- results(gp_res)
  expect_true(get_result(res, "examples_dontrun"))
})

# -- examples_all_nonrunnable -------------------------------------------------

test_that("examples_all_nonrunnable fails when all code is wrapped", {
  gp_res <- gp("bad_examples", checks = "examples_all_nonrunnable")
  res <- results(gp_res)
  expect_false(get_result(res, "examples_all_nonrunnable"))

  pos <- failed_positions(gp_res)$examples_all_nonrunnable
  expect_length(pos, 1)
})

test_that("examples_all_nonrunnable passes when runnable code exists", {
  gp_res <- gp("good", checks = "examples_all_nonrunnable")
  res <- results(gp_res)
  expect_true(get_result(res, "examples_all_nonrunnable"))
})
