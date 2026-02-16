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
