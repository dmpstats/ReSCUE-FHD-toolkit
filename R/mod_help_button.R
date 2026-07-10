#' help_button UI Function
#'
#' Renders a small info-circle icon button that, when clicked, displays a
#' markdown help file in a modal dialog. Pair with [mod_help_button_server()].
#'
#' @param id Shiny module namespace ID.
#'
#' @return A Shiny tag object containing an icon-only action button.
#'
#' @examples
#' # In a UI definition:
#' tags$div(
#'   class = "d-flex align-items-center gap-2",
#'   tags$span("Upload Data"),
#'   mod_help_button_ui("help_upload")
#' )
#'
#' @noRd
mod_help_button_ui <- function(id, type = c("button-only", "button-text", "toolbar")) {
  ns <- NS(id)

  if (type == "button-only") {
    return(
      actionButton(
        ns("help"),
        label        = NULL,
        icon         = bsicons::bs_icon("info-circle"),
        class        = "btn-help",
        `aria-label` = "Help"
      )
    )
  }

  if (type == "button-text") {
    return(
      actionButton(
        ns("help"),
        label        = "Help",
        icon         = bsicons::bs_icon("info-circle"),
        class        = "btn-help",
        `aria-label` = "Help"
      )
    )
  }

  if (type == "toolbar") {
    return(
      bslib::toolbar_input_button(
        ns("help"),
        label = "Help",
        icon = bsicons::bs_icon("info-circle"),
        `aria-label` = "Help"
      )
    )
  }

  # bslib::toolbar_input_button(
  #   ns("help"),
  #   icon = bsicons::bs_icon("info-circle"),
  #   `aria-label` = "Help"
  # )
  # actionButton(
  #   ns("help"),
  #   label        = NULL,
  #   icon         = bsicons::bs_icon("info-circle"),
  #   class        = "btn-help",
  #   `aria-label` = "Help"
  # )
}


#' help_button Server Function
#'
#' Handles the click event for a help button and displays the corresponding
#' markdown file from `inst/app/help/` in a modal dialog.
#'
#' @param id Shiny module namespace ID. Must match the `id` used in
#'   [mod_help_button_ui()].
#' @param help_file Character scalar. The filename stem (no path, no `.md`
#'   extension) of a markdown file stored in `inst/app/help/`. For example,
#'   `"data_upload"` maps to `inst/app/help/data_upload.md`.
#' @param title Character scalar or `NULL`. Optional modal title. Defaults to
#'   `NULL` (no title bar).
#' @param size Character scalar. Modal size: `"s"`, `"m"` (default), `"l"`,
#'   or `"xl"`.
#'
#' @examples
#' # In app_server or a parent module server:
#' mod_help_button_server("help_upload", help_file = "data_upload")
#'
#' @noRd
mod_help_button_server <- function(id, help_file, title = NULL, size = "m") {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$help, {
      help_path <- app_sys("app", "help", paste0(help_file, ".md"))

      if (!file.exists(help_path)) {
        showModal(modalDialog(
          title     = "Help not found",
          tags$p(paste0("No help file found for '", help_file, "'.")),
          easyClose = TRUE,
          footer    = modalButton("Close"),
          size      = size
        ))
        return()
      }

      showModal(modalDialog(
        title     = title,
        shiny::includeMarkdown(help_path),
        easyClose = TRUE,
        footer    = modalButton("Close"),
        size      = size
      ))
    })

  })
}
