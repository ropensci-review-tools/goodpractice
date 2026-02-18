
#' Run a prep step with error handling
#'
#' Executes `fn()` inside [try()], stores the result in
#' `state[[prep_name]]`, and emits a warning if the step fails.
#' Use this when writing new prep functions to avoid duplicating
#' the try/warning boilerplate.
#'
#' @section Writing a new prep:
#' 1. Create `R/prep_<name>.R`
#' 2. Register the prep and call `run_prep_step()`:
#' ```
#' PREPS$myprep <- function(state, path = state$path, quiet) {
#'   run_prep_step(state, "myprep", function() {
#'     do_work(path)
#'   }, quiet = quiet)
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
#' @param fn A zero-argument function containing the actual work.
#'   Its return value is stored in `state[[prep_name]]`.
#' @param quiet Logical. Passed to [try()] as `silent`.
#' @return The updated `state` list.
#' @keywords internal

run_prep_step <- function(state, prep_name, fn, quiet) {
  state[[prep_name]] <- try(fn(), silent = quiet)
  if (inherits(state[[prep_name]], "try-error")) {
    warning(
      "Prep step for ", prep_name, " failed: ",
      conditionMessage(attr(state[[prep_name]], "condition")),
      call. = FALSE
    )
  }
  state
}
