
context("description")

D1 <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: What the package does.
Depends: R (>= 2.15)
"

D2 <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: What the package does.
Depends: foobar (>= 2.15)
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
