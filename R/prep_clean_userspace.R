#' @include lists.R

PREPS$clean_userspace <- function(state, path = state$path, quiet) {
  path <- normalizePath(path)

  if (quiet && requireNamespace("callr", quietly = TRUE)) {
    state$clean_userspace <- tryCatch(
      callr::r(
        function(path) {
          snapshot_dir <- function(p) {
            if (!dir.exists(p)) return(character())
            list.files(p, all.files = TRUE, recursive = TRUE, no.. = TRUE)
          }

          run_examples_base <- function(p) {
            mandir <- file.path(p, "man")
            if (!dir.exists(mandir)) return(invisible())
            rd_files <- list.files(mandir, pattern = "\\.Rd$", full.names = TRUE)
            tmpdir <- tempfile("gp_examples_")
            dir.create(tmpdir)
            on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
            for (rd in rd_files) {
              ex_file <- file.path(tmpdir, paste0(basename(rd), ".R"))
              tryCatch({
                tools::Rd2ex(rd, out = ex_file)
                if (file.exists(ex_file) && file.size(ex_file) > 0) {
                  source(ex_file, local = new.env(parent = globalenv()))
                }
              }, error = function(e) NULL)
            }
          }

          leftovers <- data.frame(
            source = character(), file = character(),
            stringsAsFactors = FALSE
          )

          before_examples <- snapshot_dir(path)
          tryCatch(
            withr::with_dir(path, run_examples_base(path)),
            error = function(e) NULL
          )
          after_examples <- snapshot_dir(path)
          new_from_examples <- setdiff(after_examples, before_examples)
          if (length(new_from_examples)) {
            leftovers <- rbind(leftovers, data.frame(
              source = "examples", file = new_from_examples,
              stringsAsFactors = FALSE
            ))
          }

          testdir <- file.path(path, "tests", "testthat")
          if (dir.exists(testdir) && requireNamespace("testthat", quietly = TRUE)) {
            before_tests <- snapshot_dir(path)
            tryCatch(
              withr::with_dir(path, testthat::test_dir(testdir, reporter = "silent")),
              error = function(e) NULL
            )
            after_tests <- snapshot_dir(path)
            new_from_tests <- setdiff(after_tests, before_tests)
            if (length(new_from_tests)) {
              leftovers <- rbind(leftovers, data.frame(
                source = "tests", file = new_from_tests,
                stringsAsFactors = FALSE
              ))
            }
          }

          leftovers
        },
        args = list(path = path),
        show = FALSE
      ),
      error = function(e) {
        warning("Prep step for clean_userspace failed.")
        structure("error", class = "try-error")
      }
    )
  } else {
    state$clean_userspace <- tryCatch({
      snapshot_dir <- function(p) {
        if (!dir.exists(p)) return(character())
        list.files(p, all.files = TRUE, recursive = TRUE, no.. = TRUE)
      }

      run_examples_base <- function(p) {
        mandir <- file.path(p, "man")
        if (!dir.exists(mandir)) return(invisible())
        rd_files <- list.files(mandir, pattern = "\\.Rd$", full.names = TRUE)
        tmpdir <- tempfile("gp_examples_")
        dir.create(tmpdir)
        on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
        for (rd in rd_files) {
          ex_file <- file.path(tmpdir, paste0(basename(rd), ".R"))
          tryCatch({
            tools::Rd2ex(rd, out = ex_file)
            if (file.exists(ex_file) && file.size(ex_file) > 0) {
              source(ex_file, local = new.env(parent = globalenv()))
            }
          }, error = function(e) NULL)
        }
      }

      leftovers <- data.frame(
        source = character(), file = character(),
        stringsAsFactors = FALSE
      )

      before_examples <- snapshot_dir(path)
      tryCatch(
        withr::with_dir(path, run_examples_base(path)),
        error = function(e) {
          if (!quiet) warning("Running examples failed: ", e$message)
        }
      )
      after_examples <- snapshot_dir(path)
      new_from_examples <- setdiff(after_examples, before_examples)
      if (length(new_from_examples)) {
        leftovers <- rbind(leftovers, data.frame(
          source = "examples", file = new_from_examples,
          stringsAsFactors = FALSE
        ))
      }

      testdir <- file.path(path, "tests", "testthat")
      if (dir.exists(testdir) && requireNamespace("testthat", quietly = TRUE)) {
        before_tests <- snapshot_dir(path)
        tryCatch(
          withr::with_dir(path, testthat::test_dir(testdir, reporter = "silent")),
          error = function(e) {
            if (!quiet) warning("Running tests failed: ", e$message)
          }
        )
        after_tests <- snapshot_dir(path)
        new_from_tests <- setdiff(after_tests, before_tests)
        if (length(new_from_tests)) {
          leftovers <- rbind(leftovers, data.frame(
            source = "tests", file = new_from_tests,
            stringsAsFactors = FALSE
          ))
        }
      }

      leftovers
    }, error = function(e) {
      warning("Prep step for clean_userspace failed.")
      structure("error", class = "try-error")
    })
  }

  state
}
