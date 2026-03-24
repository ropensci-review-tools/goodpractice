test_that("print with default and explicit positions.limit", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "r_file_extension")

  testthat::expect_snapshot(print(x))
})

test_that("print shows praise when all checks pass", {
  gp_res <- gp("good", checks = "description_bugreports")
  testthat::expect_snapshot(print(gp_res))
})

test_that("gp_positions truncates when exceeding limit", {
  pos <- lapply(1:8, function(i) {
    list(
      filename = paste0("R/file", i, ".R"),
      line_number = i * 10,
      column_number = NA_integer_,
      ranges = list(),
      line = paste0("line ", i)
    )
  })

  tmp <- withr::local_tempdir()
  testthat::expect_snapshot(
    withr::with_dir(tmp, gp_positions(pos, limit = 3))
  )
})

test_that("gp_positions handles NA line_number", {
  pos <- list(list(
    filename = "R/test.R",
    line_number = NA_integer_,
    column_number = NA_integer_,
    ranges = list(),
    line = "test line"
  ))

  tmp <- withr::local_tempdir()
  testthat::expect_snapshot(
    withr::with_dir(tmp, gp_positions(pos, limit = 5))
  )
})

test_that("gp_positions includes column when available", {
  pos <- list(list(
    filename = "R/test.R",
    line_number = 10L,
    column_number = 5L,
    ranges = list(),
    line = "test line"
  ))

  tmp <- withr::local_tempdir()
  testthat::expect_snapshot(
    withr::with_dir(tmp, gp_positions(pos, limit = 5))
  )
})

test_that("check_type extracts type from result", {
  expect_equal(check_type(TRUE), "error")
  expect_equal(check_type(list(status = TRUE)), "error")
  expect_equal(check_type(list(status = TRUE, type = "info")), "info")
  expect_equal(check_type(list(status = FALSE, type = "warning")), "warning")
})

test_that("print shows info messages with praise", {
  info_check <- make_check(
    description = "An info check",
    tags = character(),
    preps = "code_structure",
    gp = "consider doing something.",
    check = function(state) list(status = TRUE, type = "info")
  )
  gp_res <- gp(
    "good",
    checks = c("info_test", "description_bugreports"),
    extra_checks = list(info_test = info_check)
  )
  testthat::expect_snapshot(print(gp_res))
})

test_that("print calls rstudio_source_markers when hasFun is TRUE", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "r_file_extension")

  called <- FALSE
  local_mocked_bindings(
    hasFun = function(...) TRUE,
    rstudio_source_markers = function(...) {
      called <<- TRUE
    }
  )
  testthat::expect_snapshot(print(x))
  expect_true(called)
})
