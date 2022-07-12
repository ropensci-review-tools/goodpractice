
D1 <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: What the package does.
Depends: R (>= 2.15)
URL: https://www.foo.com
BugReports: https://www.foo.com/bugs
"

D2 <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: What the package does.
Depends: foobar (>= 2.15)
Date: 2018-03-22
"

test_that("Depends: R is OK", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$no_description_depends$check(state)
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$no_description_depends$check(state)
  )
})

test_that("Date", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$no_description_date$check(state)
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$no_description_date$check(state)
  )
})

test_that("URL", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$description_url$check(state)
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$description_url$check(state)
  )
})

test_that("BugReports", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$description_bugreports$check(state)
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$description_bugreports$check(state)
  )
})
