fake_db <- matrix(
  character(0), nrow = 0, ncol = 3,
  dimnames = list(NULL, c("Package", "Version", "Depends"))
)

make_desc_state <- function(pkg_name = "testpkg") {
  d <- desc::desc("!new")
  d$set(Package = pkg_name)
  list(description = d, revdep = fake_db)
}

describe("reverse_dependencies check", {

  it("passes when package has no reverse deps", {
    local_mocked_bindings(query_reverse_deps = function(pkg_name, db) NULL)
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(result)
  })

  it("returns info result when package has reverse deps", {
    local_mocked_bindings(
      query_reverse_deps = function(pkg_name, db) c("pkgA", "pkgB")
    )
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(result$status)
    expect_equal(result$type, "info")
    expect_equal(result$revdeps, c("pkgA", "pkgB"))
  })

  it("returns NA on query error", {
    local_mocked_bindings(
      query_reverse_deps = function(pkg_name, db) stop("no internet")
    )
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("returns NA when description is a try-error", {
    state <- list(
      description = try(stop("fail"), silent = TRUE),
      revdep = fake_db
    )
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("returns NA when revdep is NA", {
    state <- make_desc_state()
    state$revdep <- NA
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("returns NA when package name is missing", {
    d <- desc::desc("!new")
    d$del("Package")
    state <- list(description = d, revdep = fake_db)
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })

  it("passes when package has zero reverse deps (empty character)", {
    local_mocked_bindings(query_reverse_deps = function(pkg_name, db) character(0))
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(result)
  })

  it("returns NA when query_reverse_deps returns NA (no internet)", {
    local_mocked_bindings(query_reverse_deps = function(pkg_name, db) NA)
    state <- make_desc_state()
    result <- CHECKS$reverse_dependencies$check(state)
    expect_true(is.na(result))
  })
})

describe("revdep_gp_message", {

  it("lists reverse deps", {
    msg <- revdep_gp_message(c("pkgA", "pkgB"))
    expect_match(msg, "pkgA, pkgB")
  })

  it("uses singular for 1 reverse dep", {
    msg <- revdep_gp_message("pkgA")
    expect_match(msg, "1 reverse dependency")
  })

  it("truncates when more than 10 deps", {
    deps <- paste0("pkg", seq_len(12))
    msg <- revdep_gp_message(deps)
    expect_match(msg, "and 2 more")
  })

  it("mentions revdep_check", {
    msg <- revdep_gp_message("pkgA")
    expect_match(msg, "revdep_check")
  })
})

describe("query_reverse_deps", {

  it("returns reverse deps from provided db", {
    fake_db <- matrix(
      c("pkgA", "1.0", "testpkg"), nrow = 1,
      dimnames = list("pkgA", c("Package", "Version", "Depends"))
    )
    local_mocked_bindings(
      package_dependencies = function(pkg, db, reverse) list(testpkg = c("pkgA")),
      .package = "tools"
    )
    result <- query_reverse_deps("testpkg", fake_db)
    expect_equal(result, "pkgA")
  })

  it("returns NULL when no reverse deps", {
    fake_db <- matrix(
      character(0), nrow = 0, ncol = 3,
      dimnames = list(NULL, c("Package", "Version", "Depends"))
    )
    local_mocked_bindings(
      package_dependencies = function(pkg, db, reverse) list(testpkg = NULL),
      .package = "tools"
    )
    result <- query_reverse_deps("testpkg", fake_db)
    expect_null(result)
  })
})

describe("revdep prep", {

  it("returns NA when no internet", {
    local_mocked_bindings(has_internet = function() FALSE, .package = "curl")
    state <- list(path = ".")
    state <- PREPS$revdep(state, quiet = TRUE)
    expect_true(identical(state$revdep, NA))
  })
})
