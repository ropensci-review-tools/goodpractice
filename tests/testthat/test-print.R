test_that("print with default and explicit positions.limit", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "r_file_extension")

  expect_snapshot(print(x))
})

test_that("print shows praise when all checks pass", {
  gp_res <- gp("good", checks = "description_bugreports")
  expect_snapshot(print(gp_res))
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
  expect_snapshot(
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
  expect_snapshot(
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
  expect_snapshot(
    withr::with_dir(tmp, gp_positions(pos, limit = 5))
  )
})

test_that("gp_print_groups returns all checks when groups is NULL", {
  checks <- c("description_bugreports", "no_description_date", "r_file_extension")
  expect_identical(gp_print_groups(checks, groups = NULL), checks)
})

test_that("gp_print_groups filters checks to a single group", {
  all_desc <- checks_by_group("description")
  other <- "r_file_extension"
  input <- c(all_desc, other)
  result <- gp_print_groups(input, groups = "description")
  expect_true(all(result %in% all_desc))
  expect_false(other %in% result)
})

test_that("gp_print_groups filters checks to multiple groups", {
  desc <- checks_by_group("description")
  ns <- checks_by_group("namespace")
  other <- "r_file_extension"
  input <- c(desc, ns, other)
  result <- gp_print_groups(input, groups = c("description", "namespace"))
  expect_true(all(result %in% c(desc, ns)))
  expect_false(other %in% result)
})

test_that("gp_print_groups errors on invalid group", {
  expect_error(gp_print_groups(character(0), groups = "not_a_real_group"))
})

test_that("print with groups filters output to that group", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = c("r_file_extension", "description_bugreports"))
  expect_snapshot(print(x, groups = "description"))
})

test_that("print with invalid groups errors", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "r_file_extension")
  expect_error(print(x, groups = "not_a_real_group"))
})

test_that("check_type extracts type from result", {
  expect_identical(check_type(TRUE), "error")
  expect_identical(check_type(list(status = TRUE)), "error")
  expect_identical(check_type(list(status = TRUE, type = "info")), "info")
  expect_identical(check_type(list(status = FALSE, type = "warning")), "warning")
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
  expect_snapshot(print(gp_res))
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
  expect_snapshot(print(x))
  expect_true(called)
})
