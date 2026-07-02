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
    p("Dummy content below"),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      textInput(ns("species_id"), "Species"),
      selectInput(
        ns("season"),
        "Season",
        choices = c("Spring", "Summer", "Autumn", "Winter")
      ),
      selectInput(
        ns("region"),
        "Region",
        choices = c("North", "South", "East", "West")
      )
    ),
    bslib::card_footer(
      bslib::layout_columns(
        col_widths = c(6, 6),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            ns("clear_selection"),
            label = tagList(
              bsicons::bs_icon("x-circle"),
              "Clear User Uploads"
            ),
            class = "btn-light"
            # style = "width:75"
          )
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            ns("add_to_selection"),
            label = tagList(
              bsicons::bs_icon("plus-circle"),
              "Add to Selection"
            ),
            class = "btn-primary"
            # style = "width:220px"
          )
        )
      )
    )
  )
}

#' user_upload Server Functions
#'
#' @noRd
mod_user_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # On start, create an empty reactiveVal to hold the list of datasets
    outdata <- reactiveVal(list())

    # When the user clicks "Add to Selection", we add the current inputs to the list of datasets
    observeEvent(input$add_to_selection, {
      new_entry <- list(
        species_id = input$species_id,
        method = "user-upload",
        season = input$season,
        region = input$region
      )
      new_list <- outdata()
      new_list[[length(new_list) + 1]] <- new_entry
      outdata(new_list)
    })

    # Clear selection when requested
    observeEvent(input$clear_selection, {
      outdata(list())
    })


    return(outdata)
  })
}
