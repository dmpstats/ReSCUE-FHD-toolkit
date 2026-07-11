#' tutorial
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import conductor
#'
#' @noRd
get_app_guide <- function() {
  Conductor$new()$step(
    #el = "tutorial_selectdata",
    title = "Hello!",
    text = "Welcome to the ReSCUE-FHD Toolkit. This tutorial will guide you through the main features of the app. You can skip it at any time by clicking the 'Skip Tutorial' button."
  )$step(
    el = "#app-version-container",
    title = "App Version",
    text = "The app version is displayed here. This is useful for troubleshooting and ensuring you are using the latest version of the app."
  )$step(
    el = "[data-value='nav-home']",
    title = "Navigation Bar",
    text = "The navigation bar allows you to switch between different sections of the app. You can click on the icons to navigate to the desired section."
  )$step(
    el = "#tutorial_selectdata",
    title = "Select Data",
    text = "Click this button to go to the Data Selection tab, where you can choose the data you want to analyze. This is the first step in using the app.",
    canClickTarget = FALSE
  )
}
