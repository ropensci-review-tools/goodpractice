test_that("describe_check", {
  expect_length(describe_check(), 0L)
  chk1 <- "rcmdcheck_non_portable_makevars"
  d1 <- describe_check(chk1)
  expect_type(d1, "list")
  expect_length(d1, length(chk1))
  expect_named(d1, chk1)

  chks4 <- c(
    "no_description_depends",
    "lintr_assignment_linter",
    "no_import_package_as_a_whole",
    "rcmdcheck_missing_docs"
  )
  d4 <- describe_check(chks4)
  expect_type(d4, "list")
  expect_length(d4, length(chks4))
  expect_named(d4, chks4)

  chks6 <- c(chks4, "a", "b") # "a", "b" are not checks, so ignored
  d6 <- describe_check(chks6)
  expect_type(d6, "list")
  expect_false(length(d6) == length(chks6))
  expect_identical(d6, d4)
})
