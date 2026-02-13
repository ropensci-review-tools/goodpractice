make_rcmd_state <- function(
  warnings = character(),
  notes = character(),
  errors = character()
) {
  list(
    rcmdcheck = list(
      warnings = warnings,
      notes = notes,
      errors = errors
    )
  )
}

test_that("make_rcmd_check creates a working check", {
  chk <- make_rcmd_check(
    description = "Test check",
    pattern = "test pattern",
    type = "notes"
  )
  expect_true(is.function(chk$check))
  expect_true(is.function(chk$gp))
  expect_true("rcmdcheck" %in% chk$preps)
  expect_true("info" %in% chk$tags)
})

test_that("make_rcmd_check check passes when no match", {
  chk <- make_rcmd_check(
    description = "Test",
    pattern = "bad stuff",
    type = "notes"
  )
  state <- make_rcmd_state(notes = "Everything is fine")
  expect_true(chk$check(state))
})

test_that("make_rcmd_check check fails when pattern matches", {
  chk <- make_rcmd_check(
    description = "Test",
    pattern = "bad stuff",
    type = "notes"
  )
  state <- make_rcmd_state(notes = "found bad stuff in code")
  expect_false(chk$check(state))
})

test_that("make_rcmd_check check returns NA on try-error", {
  chk <- make_rcmd_check(
    description = "Test",
    pattern = "anything",
    type = "warnings"
  )
  state <- list(rcmdcheck = structure("error", class = "try-error"))
  expect_true(is.na(chk$check(state)))
})

test_that("make_rcmd_check gp returns advice", {
  chk <- make_rcmd_check(
    description = "Test",
    pattern = "bad stuff",
    type = "notes"
  )
  state <- make_rcmd_state(
    notes = "checking ... NOTE found bad stuff"
  )
  msg <- chk$gp(state)
  expect_type(msg, "character")
  expect_true(grepl("fix this R CMD check NOTE", msg))
})

test_that("make_rcmd_check handles warning type", {
  chk <- make_rcmd_check(
    description = "Test",
    pattern = "oops",
    type = "warnings"
  )
  expect_true("warning" %in% chk$tags)

  state <- make_rcmd_state(warnings = "checking ... WARNING oops")
  expect_false(chk$check(state))
  expect_true(grepl("WARNING", chk$gp(state)))
})

test_that("make_rcmd_check handles error type", {
  chk <- make_rcmd_check(
    description = "Test",
    pattern = "fatal",
    type = "errors"
  )
  expect_true("error" %in% chk$tags)

  state <- make_rcmd_state(errors = "checking ... ERROR fatal crash")
  expect_false(chk$check(state))
  expect_true(grepl("ERROR", chk$gp(state)))
})
