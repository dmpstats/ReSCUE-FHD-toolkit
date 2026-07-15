#' table_picker UI Function
#'
#' @description A shiny Module allowing users to select from a list of non-mapped flight-height distributions.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_picker_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Select rows with checkboxes to add entries:"),
    DT::DTOutput(ns("source_table")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      div(
        class = "d-flex justify-content-center",
        actionButton(
          ns("clear_selection"),
          label = tagList(
            bsicons::bs_icon("x-circle"),
            "Clear Selection"
          ),
          class = "btn-secondary",
          style = "width:20vw"
        )
      ),
      div(
        class = "d-flex justify-content-center",
        actionButton(
          ns("add_to_selection"),
          label = tagList(
            bsicons::bs_icon("plus-circle"),
            "Add Selected Datasets"
          ),
          class = "btn-primary",
          style = "width:20vw"
        )
      )
    )
  )
}

#' table_picker Server Functions
#'
#' @noRd
mod_table_picker_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize with an empty reactive value to store the selected data
    outdata <- reactiveVal(list())

    # Dummy table: random offshore points around the UK
    set.seed(3847)
    n_pts <- 10
    dummy_pts <- data.frame(
      # lat = runif(n_pts, 49.5, 61.5),
      # lon = runif(n_pts, -8.5, 2.5),
      species_id = sample(
        c("Puffin", "Guillemot", "Bald Eagle"),
        n_pts,
        replace = TRUE
      ),
      method = "table-picker",
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

    # Present this in a DT with checkboxes
    output$source_table <- DT::renderDataTable({
      DT::datatable(
        dummy_pts,
        selection = list(mode = "multiple", selected = c()),
        options = list(
          pageLength = 10,
          autoWidth = FALSE,
          columnDefs = list(
            list(
              targets = "_all",
              render = DT::JS("function(data, type) { return data; }")
            )
          )
        ),
        rownames = FALSE
      )
    })

    # When "Add Selected Rows" is clicked
    observeEvent(input$add_to_selection, {
      selected_rows <- input$source_table_rows_selected
      if (!is.null(selected_rows) && length(selected_rows) > 0) {
        selected_data <- dummy_pts[selected_rows, ]
        current_data <- outdata()
        # Convert each selected row to a list entry
        for (i in seq_len(nrow(selected_data))) {
          current_data[[length(current_data) + 1]] <- as.list(selected_data[
            i,
          ])
        }
        outdata(current_data)
      }
    })

    # Clear selection button
    observeEvent(input$clear_selection, {
      outdata(list())
    })

    return(outdata)
  })
}

## To be copied in the UI
# mod_table_picker_ui("table_picker_1")

## To be copied in the server
# mod_table_picker_server("table_picker_1")
