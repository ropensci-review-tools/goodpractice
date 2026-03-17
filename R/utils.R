
get_package_name <- function(path = ".") {
  state <- parseNamespaceFile(basename(path), file.path(path, ".."))
}

`%||%` <- function(l, r) { if (is.null(l)) r else l }

#' Default pattern for R files
#' @return Regular expression.
#' @keywords internal

default_r_file_pattern <- function() {
  "\\.[RrSs]$"
}

trim_ws <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

loaded_pkg_version <- function(pkg) {
  asNamespace(pkg)$`.__NAMESPACE__.`$spec[["version"]]
}

drop_nulls <- function(l) {
  l[ ! vapply(l, is.null, TRUE) ]
}

has_internet <- function() {
  curl::has_internet()
}

safe_parse <- function(file = NULL, text = NULL, keep_source = TRUE,
                       encoding = "UTF-8") {
  args <- if (!is.null(file)) {
    list(file = file, keep.source = keep_source, encoding = encoding)
  } else {
    list(text = text, keep.source = keep_source)
  }

  tryCatch(
    do.call(base::parse, args),
    error = function(e) {
      if (keep_source) {
        args$keep.source <- FALSE
        tryCatch(do.call(base::parse, args), error = function(e) NULL)
      } else {
        NULL
      }
    }
  )
}

parse_package_functions <- function(path, keep_source = TRUE) {
  rdir <- file.path(path, "R")
  if (!dir.exists(rdir)) return(list())

  enc <- tryCatch(
    desc::desc_get_field("Encoding", default = "UTF-8", file = path),
    error = function(e) "UTF-8"
  )

  rfiles <- list.files(rdir, pattern = "\\.[rR]$", full.names = TRUE)
  result <- list()

  for (f in rfiles) {
    exprs <- safe_parse(file = f, keep_source = keep_source, encoding = enc)
    if (is.null(exprs) || length(exprs) == 0) next

    srcrefs <- attr(exprs, "srcref")

    for (i in seq_along(exprs)) {
      e <- exprs[[i]]
      if (!is.call(e)) next

      op <- deparse(e[[1]])
      if (!(op %in% c("<-", "=")) || length(e) != 3) next
      if (!is.call(e[[3]])) next
      if (!identical(deparse(e[[3]][[1]]), "function")) next

      name <- deparse(e[[2]])
      line <- if (!is.null(srcrefs) && !is.null(srcrefs[[i]])) {
        srcrefs[[i]][1]
      } else {
        NA_integer_
      }

      result[[length(result) + 1]] <- list(
        name = name,
        file = f,
        line = line,
        body = e[[3]][[3]]
      )
    }
  }

  result
}
