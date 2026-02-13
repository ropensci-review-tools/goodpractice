test_that("gp errors when DESCRIPTION is missing", {
  fake_pkg <- withr::local_tempdir("not_a_pkg")
  expect_error(gp(fake_pkg), "must be a package")
})

test_that("check_passed handles list with status field", {
  expect_true(check_passed(list(status = TRUE)))
  expect_false(check_passed(list(status = FALSE)))
  expect_true(is.na(check_passed(list(status = NA))))
})

test_that("check_passed handles bare logical", {
  expect_true(check_passed(TRUE))
  expect_false(check_passed(FALSE))
})

test_that("check_passed na_as_passed treats NA as pass", {
  expect_true(check_passed(list(status = NA), na_as_passed = TRUE))
  expect_true(check_passed(NA, na_as_passed = TRUE))
})

test_that("check_failed is the inverse of check_passed", {
  expect_true(check_failed(list(status = FALSE)))
  expect_false(check_failed(list(status = TRUE)))
})
