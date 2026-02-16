test_that("print with default and explicit positions.limit", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "truefalse_not_tf")

  testthat::expect_snapshot(print(x))
})

test_that("print shows praise when all checks pass", {
  gp_res <- gp("good", checks = "description_bugreports")
  expect_output(print(gp_res), "package", ignore.case = TRUE)
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
  expect_output(
    withr::with_dir(tmp, gp_positions(pos, limit = 3)),
    "and 5 more"
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
  expect_output(
    withr::with_dir(tmp, gp_positions(pos, limit = 5)),
    "R/test\\.R"
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
  expect_output(
    withr::with_dir(tmp, gp_positions(pos, limit = 5)),
    "10:5"
  )
})

test_that("print calls rstudio_source_markers when hasFun is TRUE", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "truefalse_not_tf")

  called <- FALSE
  local_mocked_bindings(
    hasFun = function(...) TRUE,
    rstudio_source_markers = function(...) {
      called <<- TRUE
    }
  )
  expect_output(print(x))
  expect_true(called)
})
