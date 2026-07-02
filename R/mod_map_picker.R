#' map_picker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_picker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("source_map"), height = "300px"),
    fluidRow(
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        textInput(ns("species_id"), "Species"),
        textInput(ns("region"), "Region"),
        textInput(ns("something_else"), "Something else")
      )
    )
  )
}

#' map_picker Server Functions
#' @noRd
mod_map_picker_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dummy map: random offshore points around the UK
    set.seed(3847)
    n_pts <- 120
    dummy_pts <- data.frame(
      lat = runif(n_pts, 49.5, 61.5),
      lon = runif(n_pts, -8.5, 2.5)
    )

    output$source_map <- leaflet::renderLeaflet({
      leaflet::leaflet(dummy_pts) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) |>
        leaflet::setView(lng = -3.5, lat = 56, zoom = 5) |>
        leaflet::addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          radius = 9,
          color = "#cccccc",
          weight = 1,
          fillColor = "#c8c8c8",
          fillOpacity = 0.75
        )
    })

    # Return a reactive list of the selected inputs. For now, a dummy
    outdata <- reactive({
        data.frame(
          species_id = input$species_id,
          method = "map-picker",
          region = NA
      )
    })
    return(outdata)
  })
}

## To be copied in the UI
# mod_map_picker_ui("map_picker_1")

## To be copied in the server
# mod_map_picker_server("map_picker_1")
