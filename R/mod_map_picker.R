#' map_picker UI Function
#'
#' @description A shiny module allowing users to select from a set of map-populated distributions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_picker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
    leaflet::leafletOutput(ns("source_map"), 
    height = "50vh"
  ),
  class = "rounded-box"
  ),
  br(),
  bslib::layout_columns(
    col_widths = c(6, 6),
    div(
      class = "d-flex justify-content-center",
      actionButton(
        ns("clear_selection"),
        label = tagList(
          bsicons::bs_icon("x-circle"),
          "Clear Selected Data"
        ),
        class = "btn-light"
        # style = "width:75"
      )
    ),
    div(
      class = "d-flex justify-content-center",
      actionButton(
        ns("does_nothing"),
        label = tagList(
          bsicons::bs_icon("plus-circle"),
          "Does Nothing"
        ),
        class = "btn-primary"
        # style = "width:220px"
      )
    )
  ),
    shinyjs::useShinyjs(),
    # Hidden input to track which marker was clicked
    shinyjs::hidden(shiny::numericInput(ns("clicked_marker"), "", value = NULL))
  )
}

#' map_picker Server Functions
#' @noRd
mod_map_picker_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initially, create an empty output reactive
    outdata <- reactiveVal(list())

    # Dummy map: random offshore points around the UK
    set.seed(3847)
    n_pts <- 120
    dummy_pts <- data.frame(
      lat = runif(n_pts, 49.5, 61.5),
      lon = runif(n_pts, -8.5, 2.5),
      species_id = sample(
        c("Puffin", "Guillemot", "Bald Eagle"),
        n_pts,
        replace = TRUE
      ),
      method = "map-picker",
      season = sample(
        c("Spring", "Summer", "Autumn", "Winter"),
        n_pts,
        replace = TRUE
      ),
      region = sample(
        c("North", "South", "East", "West"),
        n_pts,
        replace = TRUE
      )
    )

    output$source_map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(dummy_pts) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) |>
        leaflet::setView(lng = -3.5, lat = 56, zoom = 5)

      # Add markers with popups containing the add button
      for (i in seq_len(nrow(dummy_pts))) {
        popup_html <- paste0(
          "<div style='width: 200px;'>",
          "<strong>",
          dummy_pts$species_id[i],
          "</strong><br/>",
          "Season: ",
          dummy_pts$season[i],
          "<br/>",
          "Region: ",
          dummy_pts$region[i],
          "<br/>",
          "<button class='map-add-btn' data-row='",
          i,
          "' type='button' class='btn btn-sm btn-primary' style='margin-top: 8px; width: 100%;'>Add Entry</button>",
          "</div>"
        )

        map <- map |>
          leaflet::addCircleMarkers(
            lng = dummy_pts$lon[i],
            lat = dummy_pts$lat[i],
            radius = 9,
            color = "#0e0e0e",
            weight = 1,
            fillColor = "#c8c8c8",
            fillOpacity = 1.0,
            stroke = TRUE,
            popup = popup_html,
            popupOptions = leaflet::popupOptions(html = TRUE)
          )
      }
      map
    })

    # Handle all marker button clicks via JavaScript delegation
    shinyjs::runjs({
      paste0(
        "$(document).on('click', '.map-add-btn', function() {
          var rowIdx = $(this).attr('data-row');
          Shiny.onInputChange('",
        ns("clicked_marker"),
        "', rowIdx);
        });"
      )
    })

    # Respond to marker button clicks
    observeEvent(input$clicked_marker, {
      idx <- as.numeric(input$clicked_marker)
      if (!is.na(idx)) {
        new_entry <- as.list(dummy_pts[idx, ])
        current_data <- outdata()
        current_data[[length(current_data) + 1]] <- new_entry
        outdata(current_data)
      }
    })

    # Clear selection when requested
    observeEvent(input$clear_selection, {
      outdata(list())
    })

    # Return the accumulated data
    return(outdata)
  })
}

## To be copied in the UI
# mod_map_picker_ui("map_picker_1")

## To be copied in the server
# mod_map_picker_server("map_picker_1")
