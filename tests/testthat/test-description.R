
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

D3 <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: This package does things.
"

D3_this_is_a <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: This is a package to do things.
"

D3_the_pkg <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: The foo package does things.
"

D_url_good <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: Tools for stuff. See <https://example.com> for details.
    Based on Smith (2020) <doi:10.1234/example>.
"

D_url_bare <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: Tools for stuff. See https://example.com for details.
"

D_doi_as_url <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: Based on Smith (2020) <https://doi.org/10.1234/example>.
"

D_http <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: Tools for stuff. See <http://example.com> for details.
"

D_dup_deps <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: Does things.
Imports: dplyr
Suggests: dplyr
"

D_no_dup_deps <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: Does things.
Imports: dplyr
Suggests: testthat
"

D_valid_roles <-
"Package: foo
Title: Foo Package
Authors@R: c(
    person('A', 'B', role = c('aut', 'cre')),
    person('C', 'D', role = 'ctb'))
Description: Does things.
"

D_invalid_roles <-
"Package: foo
Title: Foo Package
Authors@R: c(
    person('A', 'B', role = c('aut', 'cre')),
    person('C', 'D', role = 'inventor'))
Description: Does things.
"

D_pkg_quoted <-
"Package: foo
Title: A 'dplyr' Extension
Description: Extends 'dplyr' with more verbs.
Imports: dplyr
"

D_pkg_unquoted <-
"Package: foo
Title: A dplyr Extension
Description: Extends dplyr with more verbs.
Imports: dplyr
"

D_base_pkg_unquoted <-
"Package: foo
Title: Foo with tools
Description: Uses tools for stuff.
Imports: tools
"

D_no_deps <-
"Package: foo
Title: Foo Package
Maintainer: foo@foofoo.com
Description: Does things.
"

D_bad_authors_at_r <-
"Package: foo
Title: Foo Package
Authors@R: stop('broken')
Description: Does things.
"

test_that("all checks return NA on try-error state", {
  state <- list(description = structure("error", class = "try-error",
    condition = simpleError("test")))

  check_names <- c(
    "no_description_depends", "no_description_date",
    "description_url", "description_not_start_with_package",
    "description_urls_in_angle_brackets", "description_doi_format",
    "description_urls_not_http", "no_description_duplicate_deps",
    "description_valid_roles", "description_pkgname_single_quoted",
    "description_bugreports"
  )

  for (nm in check_names) {
    result <- CHECKS[[nm]]$check(state)
    expect_identical(result$status, NA, label = paste(nm, "on try-error"))
    expect_identical(result$positions, list(), label = paste(nm, "positions on try-error"))
  }
})

test_that("Depends: R is OK", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$no_description_depends$check(state)$status
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$no_description_depends$check(state)$status
  )
})

test_that("Date", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$no_description_date$check(state)$status
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$no_description_date$check(state)$status
  )
})

test_that("URL", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$description_url$check(state)$status
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$description_url$check(state)$status
  )
})

test_that("Description not starting with package name reference", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$description_not_start_with_package$check(state)$status
  )

  state <- list(description = desc::description$new(text = D3))
  expect_false(
    CHECKS$description_not_start_with_package$check(state)$status
  )

  state <- list(description = desc::description$new(text = D3_this_is_a))
  expect_false(
    CHECKS$description_not_start_with_package$check(state)$status
  )

  state <- list(description = desc::description$new(text = D3_the_pkg))
  expect_false(
    CHECKS$description_not_start_with_package$check(state)$status
  )
})

test_that("URLs in Description enclosed in angle brackets", {

  state <- list(description = desc::description$new(text = D_url_good))
  expect_true(
    CHECKS$description_urls_in_angle_brackets$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_url_bare))
  expect_false(
    CHECKS$description_urls_in_angle_brackets$check(state)$status
  )
})

test_that("DOIs use <doi:...> format", {

  state <- list(description = desc::description$new(text = D_url_good))
  expect_true(
    CHECKS$description_doi_format$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_doi_as_url))
  expect_false(
    CHECKS$description_doi_format$check(state)$status
  )
})

test_that("URLs use https not http", {

  state <- list(description = desc::description$new(text = D_url_good))
  expect_true(
    CHECKS$description_urls_not_http$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_http))
  expect_false(
    CHECKS$description_urls_not_http$check(state)$status
  )
})

test_that("No duplicate dependencies", {

  state <- list(description = desc::description$new(text = D_no_dup_deps))
  expect_true(
    CHECKS$no_description_duplicate_deps$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_dup_deps))
  expect_false(
    CHECKS$no_description_duplicate_deps$check(state)$status
  )
})

test_that("Valid author roles", {

  state <- list(description = desc::description$new(text = D_valid_roles))
  expect_true(
    CHECKS$description_valid_roles$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_invalid_roles))
  expect_false(
    CHECKS$description_valid_roles$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_bad_authors_at_r))
  result <- CHECKS$description_valid_roles$check(state)
  expect_identical(result$status, NA)
  expect_identical(result$positions, list())
})

test_that("Package names single-quoted in Title/Description", {

  state <- list(description = desc::description$new(text = D_pkg_quoted))
  expect_true(
    CHECKS$description_pkgname_single_quoted$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_pkg_unquoted))
  expect_false(
    CHECKS$description_pkgname_single_quoted$check(state)$status
  )

  state <- list(description = desc::description$new(text = D_no_deps))
  expect_true(
    CHECKS$description_pkgname_single_quoted$check(state)$status
  )
})

test_that("Base R packages are excluded from single-quote check", {
  state <- list(description = desc::description$new(text = D_base_pkg_unquoted))
  expect_true(
    CHECKS$description_pkgname_single_quoted$check(state)$status
  )
})

test_that("BugReports", {

  state <- list(description = desc::description$new(text = D1))
  expect_true(
    CHECKS$description_bugreports$check(state)$status
  )

  state <- list(description = desc::description$new(text = D2))
  expect_false(
    CHECKS$description_bugreports$check(state)$status
  )
})
