
#' Run a prep step with error handling
#'
#' Executes `fn(...)` inside [try()], stores the result in
#' `state[[prep_name]]`, and emits a warning if the step fails.
#' Use this when writing new prep functions to avoid duplicating
#' the try/warning boilerplate.
#'
#' @section Writing a new prep:
#' 1. Create `R/prep_<name>.R`
#' 2. Register the prep and call `run_prep_step()`:
#' ```
#' PREPS$myprep <- function(state, path = state$path, quiet) {
#'   run_prep_step(state, "myprep", function(path) {
#'     do_work(path)
#'   }, path = path, silent = quiet)
#' }
#' ```
#' 3. If you need post-processing on success, capture the returned
#'    state and check `!inherits(state$myprep, "try-error")` before
#'    doing additional work (see `prep_covr.R` for an example).
#'
#' @param state The goodpractice state list, passed through the prep
#'   pipeline.
#' @param prep_name Character scalar. Name for this prep step, used as
#'   the key in `state` and in the warning message on failure.
#' @param fn A function containing the actual work. Receives
#'   prep-specific arguments via `...`.
#'   Its return value is stored in `state[[prep_name]]`.
#' @param ... Arguments forwarded to `fn`.
#' @param silent Logical. Passed to [try()] as `silent`.
#' @return The updated `state` list.
#' @keywords internal

#' @noRd
run_prep_step <- function(state, prep_name, fn, ..., silent = FALSE) {
  state[[prep_name]] <- try(do.call(fn, list(...)), silent = silent)
  if (inherits(state[[prep_name]], "try-error")) {
    cli::cli_warn(
      "Prep step for {.val {prep_name}} failed: {conditionMessage(attr(state[[prep_name]], 'condition'))}"
    )
  }
  state
}
