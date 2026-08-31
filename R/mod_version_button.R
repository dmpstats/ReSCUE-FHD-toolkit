#' version_button UI Function
#'
#' Renders a button displaying the app version that, when clicked, opens
#' the NEWS.md file in a popover. Pair with `mod_version_button_server()`.
#'
#' @param id Shiny module namespace ID.
#'
#' @return A Shiny tag object containing a version button.
#'
#' @examples
#' # In a UI definition:
#' mod_version_button_ui("app_version")
#'
#' @noRd
mod_version_button_ui <- function(id) {
  ns <- NS(id)
  shiny::actionButton(
    ns("version"),
    label = paste("v", golem::get_golem_version()),
    class = "btn-outline-light btn-sm",
    style = "font-size: 0.85rem; padding: 0.1rem 0.25rem;"
  ) |>
    bslib::tooltip(
      placement = "bottom",
      "See Version History"
    )
}


#' version_button Server Function
#'
#' Handles the click event for the version button and displays the NEWS.md
#' file in a popover.
#'
#' @param id Shiny module namespace ID. Must match the `id` used in
#'   `mod_version_button_ui()`.
#'
#' @examples
#' # In app_server or a parent module server:#'
#' mod_version_button_server("app_version")
#'
#' @noRd
mod_version_button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$version, {
      news_path <- app_sys("NEWS.md")

      if (!file.exists(news_path)) {
        showModal(modalDialog(
          title = "Changelog not found",
          tags$p("No NEWS.md file found."),
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "m"
        ))
        return()
      }

      showModal(modalDialog(
        title = NULL,
        shiny::includeMarkdown(news_path),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    })
  })
}
