#' table_picker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_picker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Table picker content will go here"),
    textInput(ns("species_id"), "Species")
  )
}
    
#' table_picker Server Functions
#'
#' @noRd 
mod_table_picker_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # For now, return a dummy reactive
    outdata <- reactive({
        data.frame(
          species_id = input$species_id,
          method = "table-picker",
          region = NA
        )
    })
    return(outdata)
  })
}
    
## To be copied in the UI
# mod_table_picker_ui("table_picker_1")
    
## To be copied in the server
# mod_table_picker_server("table_picker_1")
