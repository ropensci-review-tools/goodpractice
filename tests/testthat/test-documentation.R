
test_that("has_readme passes when README.md exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE),
    pkg,
    recursive = TRUE
  )
  file.create(file.path(pkg, "README.md"))

  state <- list(path = pkg)
  expect_true(CHECKS$has_readme$check(state))
})

test_that("has_readme passes when README.Rmd exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE),
    pkg,
    recursive = TRUE
  )
  file.create(file.path(pkg, "README.Rmd"))

  state <- list(path = pkg)
  expect_true(CHECKS$has_readme$check(state))
})

test_that("has_readme fails when no README", {
  state <- list(path = "good")
  expect_false(CHECKS$has_readme$check(state))
})

test_that("has_news passes when NEWS.md exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE),
    pkg,
    recursive = TRUE
  )
  file.create(file.path(pkg, "NEWS.md"))

  state <- list(path = pkg)
  expect_true(CHECKS$has_news$check(state))
})

test_that("has_news passes when inst/NEWS.Rd exists", {
  pkg <- withr::local_tempdir()
  file.copy(
    list.files("good", full.names = TRUE),
    pkg,
    recursive = TRUE
  )
  dir.create(file.path(pkg, "inst"), showWarnings = FALSE)
  file.create(file.path(pkg, "inst", "NEWS.Rd"))

  state <- list(path = pkg)
  expect_true(CHECKS$has_news$check(state))
})

test_that("has_news fails when no NEWS", {
  state <- list(path = "good")
  expect_false(CHECKS$has_news$check(state))
})
