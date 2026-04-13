bad1 <- system.file("bad1", package = "goodpractice")

# Warning already tested in test-coverage.R
x <- suppressWarnings(
  gp(bad1, checks = c("covr", "description_bugreports"))
)

test_that("checks", {
  expect_identical(checks(x), c("covr", "description_bugreports"))
})

test_that("results", {
  res <- data.frame(
    check = c("covr", "description_bugreports"),
    passed = c(NA, FALSE),
    stringsAsFactors = FALSE
  )
  expect_identical(results(x), res)
})

test_that("failed_checks returns names of failed checks", {
  fc <- failed_checks(x)
  expect_true("description_bugreports" %in% fc)
  expect_false("covr" %in% fc)
})

test_that("failed_positions returns positions for failed checks", {
  fp <- failed_positions(x)
  expect_type(fp, "list")
  expect_true("description_bugreports" %in% names(fp))
})

test_that("failed_positions returns empty list when check has no positions", {
  fp <- failed_positions(x)
  expect_identical(fp$description_bugreports, list())
})

test_that("failed_positions returns positions when check has them", {
  y <- gp(bad1, checks = "r_file_extension")
  fp <- failed_positions(y)
  expect_type(fp$r_file_extension, "list")
  expect_gt(length(fp$r_file_extension), 0)
})

test_that("get_position returns NULL when check has no positions", {
  chk <- list(passed = FALSE)
  expect_null(goodpractice:::get_position(chk))
})

test_that("export_json writes valid JSON", {
  tmp <- withr::local_tempfile(fileext = ".json")
  export_json(x, file = tmp)
  expect_true(file.exists(tmp))
  obj <- jsonlite::fromJSON(tmp)
  expect_identical(obj$package, "badpackage")
  expect_true("failures" %in% names(obj))
  expect_true("gp_version" %in% names(obj))
})

test_that("export_json pretty-prints when requested", {
  tmp <- withr::local_tempfile(fileext = ".json")
  export_json(x, file = tmp, pretty = TRUE)
  content <- expect_warning(
    readLines(tmp),
    "incomplete final line"
  )
  expect_gt(length(content), 1)
})
