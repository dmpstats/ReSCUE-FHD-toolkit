#' user_upload UI Function
#'
#' @description A shiny Module allowing users to manually upload a flight-height distribution.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("The module is here! Ooolala")
  )
}

#' user_upload Server Functions
#'
#' @noRd
mod_user_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
