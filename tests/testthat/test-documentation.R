
test_that("has_readme passes when README.md exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  file.create(file.path(pkg, "README.md"))

  state <- list(path = pkg)
  result <- CHECKS$has_readme$check(state)
  expect_true(result$status)
  expect_type(result$positions, "list")
})

test_that("has_readme passes when README.Rmd exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  file.create(file.path(pkg, "README.Rmd"))

  state <- list(path = pkg)
  result <- CHECKS$has_readme$check(state)
  expect_true(result$status)
  expect_type(result$positions, "list")
})

test_that("has_readme fails when no README", {
  state <- list(path = "good")
  result <- CHECKS$has_readme$check(state)
  expect_false(result$status)
  expect_type(result$positions, "list")
})

test_that("has_news passes when NEWS.md exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  file.create(file.path(pkg, "NEWS.md"))

  state <- list(path = pkg)
  result <- CHECKS$has_news$check(state)
  expect_true(result$status)
  expect_type(result$positions, "list")
})

test_that("has_news passes when inst/NEWS.Rd exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE, recursive = TRUE),
    pkg
  )
  dir.create(file.path(pkg, "inst"), showWarnings = FALSE)
  file.create(file.path(pkg, "inst", "NEWS.Rd"))

  state <- list(path = pkg)
  result <- CHECKS$has_news$check(state)
  expect_true(result$status)
  expect_type(result$positions, "list")
})

test_that("has_news fails when no NEWS", {
  state <- list(path = "good")
  result <- CHECKS$has_news$check(state)
  expect_false(result$status)
  expect_type(result$positions, "list")
})

