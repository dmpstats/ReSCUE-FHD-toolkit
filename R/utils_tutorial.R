#' tutorial
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import cicerone
#'
#' @noRd
get_app_guide <- function() {
  Cicerone$new()$step(
    "tutorial_selectdata",
    "First step",
    "This is an example tutorial first step."
  )
}
