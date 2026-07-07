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
  Cicerone$new()$
    step(
      "app_version_container",
      "App Version",
      "This is the current version of the app. It is updated automatically when you update the app."
    )
}




