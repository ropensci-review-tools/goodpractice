#' @include lists.R

#' @noRd
roxygen2_na_result <- function() na_result()

#' @noRd
block_is_function <- function(block) {
  cl <- block$call
  if (is.null(cl) || !is.call(cl) || length(cl) < 3) return(FALSE)
  op <- as.character(cl[[1]])
  if (!op %in% c("<-", "=", "<<-")) return(FALSE)
  if (!is.name(cl[[2]])) return(FALSE)
  rhs <- cl[[3]]
  is.call(rhs) && identical(rhs[[1]], quote(`function`))
}

#' @noRd
block_function_name <- function(block) {
  as.character(block$call[[2]])
}

#' @noRd
make_block_position <- function(block) {
  list(
    filename = file.path("R", basename(block$file)),
    line_number = as.integer(block$line),
    column_number = NA_integer_,
    ranges = list(),
    line = deparse(block$call, nlines = 1)
  )
}

# -- export / noRd tagging ----------------------------------------------------

CHECKS$roxygen2_has_export_or_nord <- make_check(

  description = "Documented functions have @export or @noRd",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",
  gp = "Tag every documented function with either {.code @export} or {.code @noRd}.",

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    problems <- list()

    documented_names <- character()
    for (block in rox$blocks) {
      if (!block_is_function(block)) next
      name <- block_function_name(block)
      documented_names <- c(documented_names, name)

      has_tag <- roxygen2::block_has_tags(block, c("export", "noRd", "rdname"))
      in_ns <- name %in% rox$namespace_exports ||
        name %in% rox$namespace_s3methods
      if (!has_tag && !in_ns) {
        problems[[length(problems) + 1]] <- make_block_position(block)
      }
    }

    for (i in seq_len(nrow(rox$function_defs))) {
      fn <- rox$function_defs[i, ]
      if (fn$name %in% documented_names) next
      if (fn$name %in% rox$namespace_exports) next
      if (fn$name %in% rox$namespace_s3methods) next
      problems[[length(problems) + 1]] <- list(
        filename = file.path("R", basename(fn$file)),
        line_number = as.integer(fn$line),
        column_number = NA_integer_,
        ranges = list(),
        line = fn$name
      )
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

# -- unknown tags -------------------------------------------------------------

CHECKS$roxygen2_unknown_tags <- make_check(

  description = "All roxygen2 tags are recognized",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",
  gp = paste(
    "Fix or remove unknown {.pkg roxygen2} tags.",
    "This may indicate a typo, a removed tag like {.code @S3method},",
    "or a custom tag from an unregistered {.pkg roxygen2} extension",
    "(e.g. a roclet or a package that creates new tags)."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    msgs <- if (is.null(rox$parse_messages)) character() else
      rox$parse_messages
    problems <- list()

    ansi_re <- "\033\\[[0-9;]*m"
    tag_re <- "([A-Za-z0-9_./-]+\\.R):(\\d+):.*@(\\S+)\\s+is not a known tag"
    for (msg in msgs) {
      clean <- gsub(ansi_re, "", msg)
      m <- regmatches(clean, regexec(tag_re, clean))[[1]]
      if (length(m) < 4) next
      raw_file <- m[2]
      fname <- if (startsWith(raw_file, "R/")) raw_file else
        file.path("R", raw_file)
      problems[[length(problems) + 1]] <- list(
        filename = fname,
        line_number = as.integer(m[3]),
        column_number = NA_integer_,
        ranges = list(),
        line = paste0("@", m[4])
      )
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

# -- @inheritParams / @inheritDotParams validation ----------------------------

CHECKS$roxygen2_valid_inherit <- make_check(

  description = "@inheritParams/@inheritDotParams reference known functions",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",
  gp = paste(
    "Ensure functions referenced by {.code @inheritParams} and {.code @inheritDotParams}",
    "exist within the package. Use {.code pkg::func} syntax for external functions."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())
    rox <- state$roxygen2
    pkg_fns <- rox$function_defs$name
    problems <- list()

    for (block in rox$blocks) {
      inherit_tags <- c(
        roxygen2::block_get_tags(block, "inheritParams"),
        roxygen2::block_get_tags(block, "inheritDotParams")
      )
      if (length(inherit_tags) == 0) next

      for (tag in inherit_tags) {
        ref <- trimws(strsplit(trimws(tag$val), "\\s+")[[1]][1])
        if (grepl("::", ref, fixed = TRUE)) next
        if (!ref %in% pkg_fns) {
          problems[[length(problems) + 1]] <- make_block_position(block)
          break
        }
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)

# -- duplicate @param documentation ------------------------------------------

#' @noRd
extract_block_params <- function(block) {
  param_tags <- roxygen2::block_get_tags(block, "param")
  if (length(param_tags) == 0) return(NULL)

  lapply(param_tags, function(tag) {
    list(
      name = tag$val$name,
      description = trimws(tag$val$description),
      file = block$file,
      line = block$line
    )
  })
}

CHECKS$roxygen2_duplicate_params <- make_check(

  description = "Avoid duplicated @param documentation across functions",
  tags = c("documentation", "roxygen2"),
  preps = "roxygen2",

  gp = paste(
    "use {.code @inheritParams} to avoid duplicating parameter documentation.",
    "Identical {.code @param} descriptions across files suggest shared docs",
    "should be inherited from a single source."
  ),

  check = function(state) {
    if (inherits(state$roxygen2, "try-error")) return(roxygen2_na_result())

    all_params <- unlist(
      lapply(state$roxygen2$blocks, extract_block_params),
      recursive = FALSE
    )
    if (is.null(all_params) || length(all_params) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    keys <- vapply(all_params, function(p) {
      paste(p$name, p$description, sep = "|||")
    }, character(1))
    files <- vapply(all_params, function(p) p$file, character(1))

    duped_keys <- unique(keys[duplicated(keys)])
    if (length(duped_keys) == 0) {
      return(list(status = TRUE, positions = list()))
    }

    problems <- list()
    for (key in duped_keys) {
      idxs <- which(keys == key)
      dup_files <- unique(basename(files[idxs]))
      if (length(dup_files) < 2) next

      for (i in idxs) {
        p <- all_params[[i]]
        problems[[length(problems) + 1]] <- list(
          filename = file.path("R", basename(p$file)),
          line_number = as.integer(p$line),
          column_number = NA_integer_,
          ranges = list(),
          line = paste0("@param ", p$name)
        )
      }
    }

    list(
      status = length(problems) == 0,
      positions = problems
    )
  }
)
