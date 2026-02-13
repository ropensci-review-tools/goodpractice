get_result <- function(res, check) res$result[res$check == check]

# -- examples_dontrun ---------------------------------------------------------

test_that("examples_dontrun fails when examples use \\dontrun", {
  gp_res <- gp("bad_examples", checks = "examples_dontrun")
  res <- results(gp_res)
  expect_false(get_result(res, "examples_dontrun"))

  pos <- failed_positions(gp_res)$examples_dontrun
  expect_true(length(pos) >= 1)
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
  expect_true(length(pos) >= 1)
})

test_that("examples_all_nonrunnable passes when runnable code exists", {
  gp_res <- gp("good", checks = "examples_all_nonrunnable")
  res <- results(gp_res)
  expect_true(get_result(res, "examples_all_nonrunnable"))
})

# -- multi-block parsing -------------------------------------------------------

test_that("consecutive @examples blocks are parsed separately", {
  blocks <- extract_examples_blocks("bad_examples")
  filenames <- vapply(blocks, `[[`, "", "filename")
  func_blocks <- blocks[grepl("functions.R", filenames)]
  expect_true(length(func_blocks) >= 4)
})

test_that("dontrun in second @examples block is detected", {
  gp_res <- gp("bad_examples", checks = "examples_dontrun")
  pos <- failed_positions(gp_res)$examples_dontrun
  expect_true(length(pos) >= 2)
})

test_that("extract_examples_blocks skips non-existent files", {
  local_mocked_bindings(
    r_package_files = function(path) c("/nonexistent/file.R")
  )
  blocks <- extract_examples_blocks("bad_examples")
  expect_length(blocks, 0)
})

test_that("extract_examples_blocks ends block at non-examples tag", {
  pkg <- withr::local_tempdir("exblock")
  dir.create(file.path(pkg, "R"))
  writeLines(
    c(
      "#' Title",
      "#' @examples",
      "#' x <- 1",
      "#' @param y ignored",
      "#' @export",
      "f <- function(y) y"
    ),
    file.path(pkg, "R", "f.R")
  )
  writeLines(
    c(
      "Package: exblock",
      "Title: Test",
      "Version: 0.0.1",
      "Description: Test."
    ),
    file.path(pkg, "DESCRIPTION")
  )

  blocks <- extract_examples_blocks(pkg)
  expect_length(blocks, 1)
  expect_equal(blocks[[1]]$lines, "x <- 1")
})
