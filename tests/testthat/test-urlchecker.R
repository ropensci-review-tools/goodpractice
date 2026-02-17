make_urlchecker_db <- function(urls = character(),
                              from = list(),
                              status = character(),
                              message = character(),
                              new = character()) {
  structure(
    data.frame(
      URL = urls,
      Status = status,
      Message = message,
      New = new,
      stringsAsFactors = FALSE
    ),
    class = c("urlchecker_db", "check_url_db", "data.frame")
  )
}

# We need to add From as a list column separately
add_from <- function(db, from) {
  db$From <- from
  db
}

# -- helpers -------------------------------------------------------------------

test_that("urlchecker_na_result returns NA status with empty positions", {
  res <- urlchecker_na_result()
  expect_true(is.na(res$status))
  expect_length(res$positions, 0)
})

test_that("urlchecker_make_positions extracts first From entry as filename", {
  db <- make_urlchecker_db(
    urls = c("https://a.com", "https://b.com"),
    status = c("404", "500"),
    message = c("Not Found", "Error"),
    new = c("", "")
  )
  db <- add_from(db, list(c("DESCRIPTION", "R/foo.R"), "man/bar.Rd"))
  pos <- urlchecker_make_positions(db)
  expect_length(pos, 2)
  expect_equal(pos[[1]]$filename, "DESCRIPTION")
  expect_equal(pos[[2]]$filename, "man/bar.Rd")
  expect_equal(pos[[1]]$line, "https://a.com")
  expect_equal(pos[[2]]$line, "https://b.com")
  expect_true(is.na(pos[[1]]$line_number))
  expect_true(is.na(pos[[1]]$column_number))
})

test_that("urlchecker_make_positions handles empty From with 'unknown'", {
  db <- make_urlchecker_db(
    urls = "https://a.com",
    status = "404",
    message = "Not Found",
    new = ""
  )
  db <- add_from(db, list(character()))
  pos <- urlchecker_make_positions(db)
  expect_equal(pos[[1]]$filename, "unknown")
})

# -- factory structure ---------------------------------------------------------

test_that("make_urlchecker_check produces a valid check object", {
  chk <- CHECKS$urlchecker_ok
  expect_s3_class(chk, "check")
  expect_equal(chk$description, "All URLs are reachable")
  expect_true("urlchecker" %in% chk$preps)
  expect_true("url" %in% chk$tags)
  expect_true(is.function(chk$check))
})

test_that("urlchecker_no_redirects has CRAN tag", {
  chk <- CHECKS$urlchecker_no_redirects
  expect_true("CRAN" %in% chk$tags)
})

# -- NULL state ----------------------------------------------------------------

test_that("urlchecker_ok passes when state$urlchecker is NULL", {
  state <- list(urlchecker = NULL)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_true(result$status)
})

test_that("urlchecker_no_redirects passes when state$urlchecker is NULL", {
  state <- list(urlchecker = NULL)
  result <- CHECKS$urlchecker_no_redirects$check(state)
  expect_true(result$status)
})

# -- urlchecker_ok ------------------------------------------------------------

test_that("urlchecker_ok passes when no broken URLs", {
  db <- make_urlchecker_db(
    urls = "https://example.com",
    status = "200",
    message = "OK",
    new = "https://example.org"
  )
  db <- add_from(db, list("DESCRIPTION"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_true(result$status)
})

test_that("urlchecker_ok fails on 404", {
  db <- make_urlchecker_db(
    urls = "https://example.com/gone",
    status = "404",
    message = "Not Found",
    new = ""
  )
  db <- add_from(db, list("DESCRIPTION"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_false(result$status)
  expect_length(result$positions, 1)
  expect_equal(result$positions[[1]]$line, "https://example.com/gone")
})

test_that("urlchecker_ok passes when all URLs return 200 or redirect", {
  db <- make_urlchecker_db(
    urls = c("https://a.com", "https://b.com"),
    status = c("200", "301"),
    message = c("OK", "Moved Permanently"),
    new = c("", "https://b.org")
  )
  db <- add_from(db, list("DESCRIPTION", "man/foo.Rd"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_true(result$status)
})

test_that("urlchecker_ok passes with empty result", {
  db <- make_urlchecker_db()
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_true(result$status)
})

test_that("urlchecker_ok returns NA on prep failure", {
  state <- list(urlchecker = structure("error", class = "try-error"))
  result <- CHECKS$urlchecker_ok$check(state)
  expect_true(is.na(result$status))
})

# -- urlchecker_no_redirects --------------------------------------------------

test_that("urlchecker_no_redirects fails when URLs redirect", {
  db <- make_urlchecker_db(
    urls = c("https://old.com", "https://fine.com"),
    status = c("200", "200"),
    message = c("OK", "OK"),
    new = c("https://new.com", "")
  )
  db <- add_from(db, list("DESCRIPTION", "man/bar.Rd"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_no_redirects$check(state)
  expect_false(result$status)
  expect_length(result$positions, 1)
  expect_equal(result$positions[[1]]$line, "https://old.com")
})

test_that("urlchecker_no_redirects passes when no redirects", {
  db <- make_urlchecker_db(
    urls = "https://fine.com",
    status = "404",
    message = "Not Found",
    new = ""
  )
  db <- add_from(db, list("DESCRIPTION"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_no_redirects$check(state)
  expect_true(result$status)
})

test_that("urlchecker_no_redirects passes with empty result", {
  db <- make_urlchecker_db()
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_no_redirects$check(state)
  expect_true(result$status)
})

test_that("urlchecker_no_redirects returns NA on prep failure", {
  state <- list(urlchecker = structure("error", class = "try-error"))
  result <- CHECKS$urlchecker_no_redirects$check(state)
  expect_true(is.na(result$status))
})

# -- position reporting -------------------------------------------------------

test_that("positions report filename from From column", {
  db <- make_urlchecker_db(
    urls = "https://broken.com",
    status = "500",
    message = "Server Error",
    new = ""
  )
  db <- add_from(db, list(c("man/foo.Rd", "R/bar.R")))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_equal(result$positions[[1]]$filename, "man/foo.Rd")
})

# -- multiple failures ---------------------------------------------------------

test_that("urlchecker_ok reports all broken URLs", {
  db <- make_urlchecker_db(
    urls = c("https://a.com", "https://b.com", "https://c.com"),
    status = c("404", "200", "500"),
    message = c("Not Found", "OK", "Server Error"),
    new = c("", "", "")
  )
  db <- add_from(db, list("DESCRIPTION", "DESCRIPTION", "man/foo.Rd"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_false(result$status)
  expect_length(result$positions, 2)
  urls <- vapply(result$positions, `[[`, "", "line")
  expect_setequal(urls, c("https://a.com", "https://c.com"))
})

test_that("urlchecker_no_redirects reports all redirecting URLs", {
  db <- make_urlchecker_db(
    urls = c("https://a.com", "https://b.com", "https://c.com"),
    status = c("200", "200", "200"),
    message = c("OK", "OK", "OK"),
    new = c("https://a.org", "", "https://c.org")
  )
  db <- add_from(db, list("DESCRIPTION", "DESCRIPTION", "man/foo.Rd"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_no_redirects$check(state)
  expect_false(result$status)
  expect_length(result$positions, 2)
})

# -- filter edge cases ---------------------------------------------------------

test_that("urlchecker_ok treats 302 as acceptable", {
  db <- make_urlchecker_db(
    urls = "https://a.com",
    status = "302",
    message = "Found",
    new = "https://a.org"
  )
  db <- add_from(db, list("DESCRIPTION"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_true(result$status)
})

test_that("urlchecker_ok catches timeout status", {
  db <- make_urlchecker_db(
    urls = "https://slow.com",
    status = "Timeout",
    message = "Connection timed out",
    new = ""
  )
  db <- add_from(db, list("DESCRIPTION"))
  state <- list(urlchecker = db)
  result <- CHECKS$urlchecker_ok$check(state)
  expect_false(result$status)
})

# -- prep tests ----------------------------------------------------------------

test_that("PREPS$urlchecker stores result in state", {
  local_mocked_bindings(has_internet = function() TRUE)
  local_mocked_bindings(
    url_check = function(path, ...) {
      make_urlchecker_db(
        urls = "https://example.com",
        status = "200",
        message = "OK",
        new = ""
      )
    },
    .package = "urlchecker"
  )
  state <- list(path = "good")
  state <- PREPS$urlchecker(state, quiet = TRUE)
  expect_false(inherits(state$urlchecker, "try-error"))
  expect_s3_class(state$urlchecker, "data.frame")
})

test_that("PREPS$urlchecker warns on failure", {
  local_mocked_bindings(has_internet = function() TRUE)
  local_mocked_bindings(
    url_check = function(path, ...) stop("url_check failed"),
    .package = "urlchecker"
  )
  state <- list(path = "good")
  expect_warning(
    state <- PREPS$urlchecker(state, quiet = TRUE),
    "Prep step for urlchecker failed"
  )
  expect_true(inherits(state$urlchecker, "try-error"))
})

# -- offline gate --------------------------------------------------------------

test_that("urlchecker checks return NA when offline", {
  local_mocked_bindings(has_internet = function() FALSE)
  expect_warning(
    gp_res <- gp("good", checks = "urlchecker_ok"),
    "Prep step for urlchecker failed"
  )
  res <- results(gp_res)
  expect_true(is.na(res$result[res$check == "urlchecker_ok"]))
})

# -- integration tests (network) ----------------------------------------------

test_that("urlchecker prep runs on good fixture", {
  skip_on_cran()
  skip_if_offline()
  gp_res <- gp("good", checks = "urlchecker_no_redirects")
  res <- results(gp_res)
  result <- res$result[res$check == "urlchecker_no_redirects"]
  expect_false(result)

  pos <- failed_positions(gp_res)$urlchecker_no_redirects
  urls <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("mangothecat", urls)))
})

test_that("urlchecker_ok passes on package with valid URLs", {
  skip_on_cran()
  skip_if_offline()
  gp_res <- gp("good", checks = "urlchecker_ok")
  res <- results(gp_res)
  result <- res$result[res$check == "urlchecker_ok"]
  expect_true(result)
})
