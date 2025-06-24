
context("describe check")

test_that("describe_check", {
    expect_length(describe_check(), 0L)
    chk <- "rcmdcheck_non_portable_makevars"
    d <- describe_check(chk)
    expect_is(d, "list")
    expect_length(d, length(chk))
    expect_named(d, chk)

    chk <- c("no_description_depends",
             "lintr_assignment_linter",
             "no_import_package_as_a_whole",
             "rcmdcheck_missing_docs")
    d <- describe_check(chk)
    expect_is(d, "list")
    expect_length(d, length(chk))
    expect_named(d, chk)

    chk <- c(chk, "a", "b") # "a", "b" are not checks, so ignored
    d <- describe_check(chk)
    expect_is(d, "list")
    expect_false(length(d) == length(chk))
    expect_false(identical(chk, names(d)))
    expect_lt(length(d), length(chk))
})
