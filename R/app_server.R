#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
	# Track Mode and switch when required
	mode <- reactiveVal("flight")
	
	# Create a reset trigger that modules can listen to
	reset_trigger <- reactiveVal(0)

	# Modules -----------------

	mod_landing_page_server(
		"landing_page",
		nav_id = "main-nav",
		parent_session = session
	)
	selected_data <- mod_data_select_server(
		"data_select",
		nav_id = "main-nav",
		parent_session = session,
		reset_trigger = reset_trigger
	)
	mod_data_analysis_server(
		"data_analysis",
		nav_id = "main-nav",
		parent_session = session,
		selected_data = selected_data,
		reset_trigger = reset_trigger
	)

	# React to page-reset button --------------
	observeEvent(
		input$reset_app,
		{
			session$reload()
		}
	)

	# React to mode-switch button --------------
	observeEvent(input$switch_mode, {
		showModal(modalDialog(
			title = "Confirm Mode Switch",
			ifelse(
				mode() == "flight",
				"This switches the app to Dive Distribution mode, which allows the analysis of dive profiles. Are you sure you want to switch to Tidal mode? This will reset the app and any unsaved data will be lost.",
				"This switches the app to Flight Height mode, which allows the analysis of flight-height profiles. Are you sure you want to switch to Flight Height mode? This will reset the app and any unsaved data will be lost."
			),
			footer = tagList(
				modalButton("Cancel"),
				actionButton("comfirm_switch", "Yes, Switch", class = "btn-info")
			)
		))
	})
	observeEvent(
		input$comfirm_switch,
		{
			shiny::removeModal()
			if (mode() == "flight") {
				mode("tidal")
			} else {
				mode("flight")
			}
			# Increment reset_trigger to tell modules to reset their data
			reset_trigger(reset_trigger() + 1)
		}
	)
	output$current_mode <- renderUI({
		actionButton(
			"switch_mode",
			label = NULL,
			class = "btn-outline-secondary",
			icon = bsicons::bs_icon(
				ifelse(mode() == "flight", "cloud", "tsunami")
			)
		) |>
			tooltip(
				ifelse(mode() == "flight", "Flight Height Mode", "Dive Distribution Mode"),
				placement = "bottom"
			)
	})

	# Helper functions
	mod_version_button_server("app_version")
}
