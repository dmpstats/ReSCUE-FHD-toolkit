#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import conductor
#' @noRd
app_server <- function(input, output, session) {
	# Track Mode and switch when required
	mode <- reactiveVal("flight")

	# Modules -----------------

	mod_landing_page_server(
		"landing_page",
		nav_id = "main-nav",
		parent_session = session
	)

	selected_data <- mod_data_select_server(
		"data_select",
		nav_id = "main-nav",
		parent_session = session
	)

	mod_data_analysis_server(
		"data_analysis",
		nav_id = "main-nav",
		parent_session = session,
		selected_data = selected_data
	)

	# React to page-reset button --------------

	observeEvent(
		input$reset_app,
		{
			session$reload()
		}
	)

	# Helper functions
	mod_version_button_server("app_version")
}
