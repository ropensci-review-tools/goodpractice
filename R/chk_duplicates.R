#' @include lists.R treesitter.R

cross_file_duplicates <- function(df, key_col, file_col) {
  duped_keys <- unique(df[[key_col]][duplicated(df[[key_col]])])
  if (length(duped_keys) == 0) return(df[0, , drop = FALSE])

  candidates <- df[df[[key_col]] %in% duped_keys, , drop = FALSE]
  multi_file <- vapply(duped_keys, function(k) {
    length(unique(basename(candidates[[file_col]][candidates[[key_col]] == k]))) >= 2
  }, logical(1))

  candidates[candidates[[key_col]] %in% duped_keys[multi_file], , drop = FALSE]
}

## --------------------------------------------------------------------

normalize_body_text <- function(fn_node) {
  body <- treesitter::node_child_by_field_name(fn_node, "body")
  if (is.null(body)) return("")
  gsub("\\s+", " ", trimws(treesitter::node_text(body)))
}

CHECKS$duplicate_function_bodies <- make_check(

  description = "No functions with identical bodies",
  tags = c("warning", "best practice"),
  preps = character(),

  gp = paste(
    "consolidate functions with identical bodies into a single",
    "shared helper to reduce code duplication."
  ),

  check = function(state) {
    ts <- ts_get(state)
    if (length(ts$functions) < 2) {
      return(list(status = TRUE, positions = list()))
    }

    bodies <- vapply(ts$functions, function(fn) {
      normalize_body_text(fn$fn_node)
    }, character(1))

    trivial <- nchar(bodies) < 20
    bodies[trivial] <- paste0("__trivial__", seq_along(bodies)[trivial])

    fn_df <- data.frame(
      body = bodies,
      name = vapply(ts$functions, `[[`, "", "name"),
      file = vapply(ts$functions, `[[`, "", "file"),
      line = vapply(ts$functions, function(fn) fn$line, numeric(1)),
      stringsAsFactors = FALSE
    )

    dupes <- cross_file_duplicates(fn_df, "body", "file")
    if (nrow(dupes) == 0) return(list(status = TRUE, positions = list()))

    problems <- lapply(seq_len(nrow(dupes)), function(i) {
      list(
        filename = file.path("R", basename(dupes$file[i])),
        line_number = dupes$line[i],
        column_number = NA_integer_,
        ranges = list(),
        line = dupes$name[i]
      )
    })

    list(status = FALSE, positions = problems)
  }
)
