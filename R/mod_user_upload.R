#' user_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("User upload content will go here"),
    textInput(ns("species_id"), "Species")
  )
}
    
#' user_upload Server Functions
#'
#' @noRd 
mod_user_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # For now, return a dummy reactive
    outdata <- reactive({
        data.frame(
          species_id = input$species_id,
          method = "user-upload",
          region = NA
        )
    })
    return(outdata)
  })
}
    
## To be copied in the UI
# mod_user_upload_ui("user_upload_1")
    
## To be copied in the server
# mod_user_upload_server("user_upload_1")
