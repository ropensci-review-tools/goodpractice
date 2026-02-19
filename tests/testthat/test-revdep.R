make_desc_state <- function(pkg_name = "testpkg") {
  d <- desc::desc("!new")
  d$set(Package = pkg_name)
  list(description = d)
}

describe("reverse_dependencies check", {

  it("passes when package has no reverse deps", {
    local_mocked_bindings(query_reverse_deps = function(pkg_name) NULL)
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(result)
  })

  it("reports reverse deps with status FALSE and positions", {
    local_mocked_bindings(
      query_reverse_deps = function(pkg_name) c("pkgA", "pkgB")
    )
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_false(result$status)
    expect_length(result$positions, 2)
    expect_equal(result$positions[[1]]$filename, "pkgA")
    expect_equal(result$positions[[2]]$filename, "pkgB")
  })

  it("returns NA on query error", {
    local_mocked_bindings(
      query_reverse_deps = function(pkg_name) stop("no internet")
    )
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("returns NA when description is a try-error", {
    state <- list(description = try(stop("fail"), silent = TRUE))
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("returns NA when package name is missing", {
    d <- desc::desc("!new")
    d$del("Package")
    state <- list(description = d)
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("gp function builds message listing reverse deps", {
    state <- list(
      checks = list(
        reverse_dependencies = list(
          status = FALSE,
          positions = list(
            list(filename = "pkgA", line_number = NA_integer_,
                 column_number = NA_integer_, ranges = list(), line = "pkgA"),
            list(filename = "pkgB", line_number = NA_integer_,
                 column_number = NA_integer_, ranges = list(), line = "pkgB")
          )
        )
      )
    )
    msg <- CHECKS$reverse_dependencies$gp(state)
    expect_match(msg, "2 reverse dependencies")
    expect_match(msg, "pkgA, pkgB")
    expect_match(msg, "revdepcheck::revdep_check")
  })

  it("gp function uses singular for 1 reverse dep", {
    state <- list(
      checks = list(
        reverse_dependencies = list(
          status = FALSE,
          positions = list(
            list(filename = "pkgA", line_number = NA_integer_,
                 column_number = NA_integer_, ranges = list(), line = "pkgA")
          )
        )
      )
    )
    msg <- CHECKS$reverse_dependencies$gp(state)
    expect_match(msg, "1 reverse dependency")
  })

  it("gp function truncates when more than 10 deps", {
    deps <- paste0("pkg", seq_len(12))
    positions <- lapply(deps, function(dep) {
      list(filename = dep, line_number = NA_integer_,
           column_number = NA_integer_, ranges = list(), line = dep)
    })
    state <- list(
      checks = list(
        reverse_dependencies = list(
          status = FALSE,
          positions = positions
        )
      )
    )
    msg <- CHECKS$reverse_dependencies$gp(state)
    expect_match(msg, "and 2 more")
    expect_false(grepl("pkg11", msg))
  })
})
