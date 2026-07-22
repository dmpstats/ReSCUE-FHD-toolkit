library(shiny)
library(bslib)

tab_analysis_app <- function() {
  ui <- tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = span("ReSCUEApp", style = "color: #ffffff;"),
      id = "main-nav",
      theme = bslib::bs_theme(
        version = 5,
        preset = "flatly",
        primary = "#002e40",
        secondary = "#b5cbca",
        success = "#ffa134",
        bg = "#113d4e",
        fg = "#ffffff"
      ) |>
        bslib::bs_add_variables(
          "border-radius" = "1rem",
          # Set the tooltip colour to Success
          "tooltip-bg" = "var(--bs-success)"
        ),
      # padding = c("1.5rem", "1.5rem", "100px", "1.5rem"),
      navbar_options = bslib::navbar_options(
        style = "height: 3rem;",
        #	position = "fixed-bottom",
        underline = FALSE
      ),
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = "",
        icon = bsicons::bs_icon(
          "bar-chart-fill",
          size = "1.5em"
        ) |>
          bslib::tooltip(
            placement = "bottom",
            "Analysis"
          ),
        value = "nav-analysis",
        mod_data_analysis_ui("data_analysis")
      )
    )
  )

  server <- function(input, output, session) {
    browser()

    selected_data <- list.files("data-dummy/draws", full.names = TRUE) |>
      purrr::map(readRDS) |>
      shiny::reactiveVal()

    mod_data_analysis_server(
      "data_analysis",
      nav_id = "main-nav",
      parent_session = session,
      selected_data = data_select_output$selected_data
    )
  }

  shinyApp(ui = ui, server = server)
}


tab_analysis_app()
