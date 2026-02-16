bad1 <- system.file("bad1", package = "goodpractice")

# Warning already tested in test-coverage.R
x <- suppressWarnings(
  gp(bad1, checks = c("covr", "description_bugreports"))
)

test_that("checks", {
  expect_equal(checks(x), c("covr", "description_bugreports"))
})

test_that("results", {
  res <- data.frame(
    check = c("covr", "description_bugreports"),
    result = c(NA, FALSE),
    stringsAsFactors = FALSE
  )
  expect_equal(results(x), res)
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

test_that("export_json writes valid JSON", {
  tmp <- withr::local_tempfile(fileext = ".json")
  export_json(x, file = tmp)
  expect_true(file.exists(tmp))
  obj <- jsonlite::fromJSON(tmp)
  expect_equal(obj$package, "badpackage")
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
  expect_true(length(content) > 1)
})
